#' read_dts_xml
#'
#' @param file_path path to the file
#' @param trim whether to trim the length of the cable
#'
#' @return dts table of results
#' @export
#'
#' @examples
read_dts_xml <- function(file_path, trim = TRUE) {
  
  x <- xmlParse(file_path)
  r <- xmlRoot(x)
  
  dts <- list(meta = read_dts_xml_meta(r), 
              data = read_dts_xml_data(r))
  
  if (trim){
    dts$data <- dts$data[distance < (dts$meta$fibre_length)]
  }
  
  return(dts)
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
#' @param trim whether to trim the length of the cable
#' @return
#' @export
#'
#' @examples
read_dts <- function(in_path, n_cores = 1, trim = TRUE) {
  
  # read single file
  if(file_test('-f', in_path)) {
    return(lapply(in_path, read_dts_xml, trim = trim))
  }
  
  # read directory
  xml_files <- list.files(in_path, 
                          pattern = '\\.xml$', 
                          full.names = TRUE)
  
  
  if(n_cores == 1) {
    return(lapply(xml_files, read_dts_xml, trim = trim))
  } else {
    # Initiate cluster
    cl <- makePSOCKcluster(n_cores)
    
    clusterEvalQ(cl, { 
      library(XML) 
      library(data.table)})
    clusterExport(cl, c('read_dts_xml', 'read_dts_xml_data', 'read_dts_xml_meta', 'trim'))
    
    res <- parallel::parLapply(cl, xml_files, read_dts_xml, trim = trim)
    
    stopCluster(cl)
  }
  
  return(res)
}


#' write_dts
#'
#' @param in_path input directory
#' @param out_path output directory
#' @param n_cores how many cores
#' @param trim whether to trim the length of the cable
#' 
#' @return
#' @export
#'
#' @examples
write_dts <- function(in_path, out_path, n_cores = 1, trim = TRUE) {

  dts  <- read_dts(in_path, n_cores, trim)

  
  write_fst(rbindlist(map(dts, function(x) x$meta)), 
            paste0(out_path, 'meta.fst'),
            compress = 50)
  write_fst(rbindlist(map(dts, function(x) x$data)), 
            paste0(out_path, 'data.fst'),
            compress = 50)
  
}



