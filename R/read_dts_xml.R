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

  
#' read_dts
#'
#' @param in_path location of the xml files
#' @param n_cores specify the number of cores
#'
#' @return
#' @export
#'
#' @examples
read_dts <- function(in_path, n_cores = 1) {
  
  # read single file
  if(file_test('-f', in_path)) {
    return(lapply(in_path, read_dts_xml))
  }
  
  # read directory
  xml_files <- list.files(in_path, 
                          pattern = '\\.xml$', 
                          full.names = TRUE)
  
  
  if(n_cores == 1) {
    return(lapply(xml_files, read_dts_xml))
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


#' write_dts
#'
#' @param in_path input directory
#' @param out_path output directory
#' @param n_cores how many cores
#'
#' @return
#' @export
#'
#' @examples
write_dts <- function(in_path, out_path, n_cores = 1) {
  dts  <- read_dts(in_path, n_cores)
  
  write_fst(rbindlist(map(dts, function(x) x$meta)), 
            paste0(out_path, 'meta.fst'),
            compress = 50)
  write_fst(rbindlist(map(dts, function(x) x$data)), 
            paste0(out_path, 'data.fst'),
            compress = 50)
}


#' trim_dts
#'
#' @param xml_dat xml data read from read_dts 
#'
#' @return
#' @export
#'
#' @examples
trim_dts <- function(xml_dat) {
  map(xml_dat, function(x){
    x$data[, `:=`(probe_1 = x$meta$probe_1,
                 probe_2 = x$meta$probe_2)][distance < (x$meta$fibre_length)]
  })
}
