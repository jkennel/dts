#' read_dts_xml
#'
#' @param file_path path to the file
#'
#' @return dts table of results
#' @export
#'
#' @examples
read_dts_xml <- function(file_path) {
  
  x <- xmlParse(file_path)
  r <- xmlRoot(x)
  
  return(list(meta = read_dts_xml_meta(r), 
              data = read_dts_xml_data(r)))
}



#' read_dts_xml_meta
#'
#' @param r root node of xml file
#'
#' @return
#' @export
#'
#' @examples
read_dts_xml_meta <- function(r) {
  
  # get times
  start <- as.POSIXct(getChildrenStrings(r[['wellLog']][['minDateTimeIndex']]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
  end   <- as.POSIXct(getChildrenStrings(r[['wellLog']][['maxDateTimeIndex']]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
  
  # is cable double ended 
  double_ended <- as.integer(getChildrenStrings(r[['wellLog']][['customData']][['isDoubleEnded']]))
  
  # get reference temperatures
  probe_1  <- as.numeric(getChildrenStrings(r[['wellLog']][['customData']][['probe1Temperature']]))
  probe_2  <- as.numeric(getChildrenStrings(r[['wellLog']][['customData']][['probe2Temperature']]))
  
  # max fibre length
  fibre_length <- as.numeric(getChildrenStrings(r[['wellLog']][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']]))
  
  return(data.table(start, end, double_ended, probe_1, probe_2, fibre_length))
  
}

#' read_dts_xml_data
#'
#' @param r root node of xml file
#'
#' @return
#' @export
#'
#' @examples
read_dts_xml_data <- function(r) {
  
  # get times
  start <- as.POSIXct(getChildrenStrings(r[['wellLog']][['minDateTimeIndex']]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
  
  # is cable double ended 
  double_ended <- as.integer(getChildrenStrings(r[['wellLog']][['customData']][['isDoubleEnded']]))
  
  # get data
  text <- paste(getChildrenStrings(r[['wellLog']][['logData']], 
                                   addNames = FALSE,
                                   asVector = TRUE),
                collapse = '\n')
  
  if (double_ended) {
    dat  <- data.table::fread(text, blank.lines.skip = TRUE, select = c(1, 6))
  } else {
    dat  <- data.table::fread(text, blank.lines.skip = TRUE, select = c(1, 4))
  }
  
  return(data.table::data.table(distance = dat[[1]],
                                temp = dat[[2]], 
                                start = start))
  
}

  
#' read_dts_folder
#'
#' @param in_dir location of the xml files
#' @param out_dir location of the xml files
#' @param n_cores specify the number of cores
#'
#' @return
#' @export
#'
#' @examples
read_dts_folder <- function(in_dir, n_cores) {
  xml_files <- list.files(in_dir, 
                          pattern = '\\.xml$', 
                          full.names = TRUE)
  
  
  if(n_cores == 1) {
    res <- lapply(xml_files, read_dts_xml)
  } else {
    # Initiate cluster
    cl <- makePSOCKcluster(n_cores)
    
    clusterEvalQ(cl, { 
      library(XML) 
      library(data.table)})
    clusterExport(cl, c('read_dts_xml', 'read_dts_xml_data', 'read_dts_xml_meta'))
    
    res <- parallel::parLapply(cl, xml_files, read_dts_xml)
    
    stopCluster(cl)
  }
  
  return(res)
}


#' write_dts_xml
#'
#' @param in_dir input directory
#' @param out_dir output directory
#' @param n_cores how many cores
#'
#' @return
#' @export
#'
#' @examples
write_dts_xml <- function(in_dir, out_dir, n_cores) {
  dts  <- read_dts_folder(in_dir, n_cores)
  
  write_fst(rbindlist(map(dts, function(x) x$meta)), 
            paste0(out_dir, 'meta.fst'),
            compress = 50)
  write_fst(rbindlist(map(dts, function(x) x$data)), 
            paste0(out_dir, 'data.fst'),
            compress = 50)
}


