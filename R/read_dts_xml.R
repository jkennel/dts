#' read_dts_xml
#'
#' @param file_path path to the file
#'
#' @return dts table of results
#' @export
#'
#' @examples
read_dts_xml <- function(file_path, wh) {

  r <- xmlRoot(xmlParse(file_path))
  
  return(list(meta = read_dts_xml_meta(r), 
              data = read_dts_xml_data(r, wh)))
}

#' read_one
#'
#' @param file_path path to the file
#'
#' @return subset vector
#' @export
#'
#' @examples
read_one <- function(file_path) {
  r <- xmlRoot(xmlParse(file_path))
  limits <- c(0, read_dts_xml_meta(r)$fibre_length)
  wh <- which(read_dts_xml_data(r)$distance %between% limits)
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
read_dts_xml_data <- function(r, wh) {
  
  # get times
  start <- as.POSIXct(getChildrenStrings(r[['wellLog']][['minDateTimeIndex']]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
  
  # max fibre length
  fibre_length <- as.numeric(getChildrenStrings(r[['wellLog']][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']]))
  
  # is cable double ended 
  double_ended <- as.integer(getChildrenStrings(r[['wellLog']][['customData']][['isDoubleEnded']]))
  if (double_ended) {
    select <- c(1, 6)
  }
  
  # get data
  text <- paste(getChildrenStrings(r[['wellLog']][['logData']], 
                                   addNames = FALSE,
                                   asVector = TRUE),
                collapse = '\n')
  
  
  text  <-   data.table::fread(text, 
                               blank.lines.skip = TRUE, 
                               select = select, 
                               sep = ',', 
                               nThread = 1, 
                               strip.white = FALSE)
  
  setnames(text, c('distance', 'temperature'))
  
  return(text)
  
}

  
#' read_dts
#'
#' @param in_path location of the xml files
#' @param n_cores specify the number of cores
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
  
  wh <- read_one(xml_files[1])
  
  if(n_cores == 1) {
    return(lapply(xml_files, read_dts_xml))
  } else {
    # Initiate cluster
    cl <- makePSOCKcluster(n_cores)
    
    clusterEvalQ(cl, { 
      library(XML) 
      library(data.table)})
    clusterExport(cl = cl, list('read_dts_xml', 
                                'read_dts_xml_data', 
                                'read_dts_xml_meta'))
    
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
#' @param one_file output a single file
#' @param trim output to exclude certain distances
#' 
#' @return
#' @export
#'
#' @examples
write_dts <- function(in_path, out_path, n_cores = 1, 
                      one_file = TRUE, trim = TRUE) {

  dts  <- read_dts(in_path, n_cores)

  # if one file is desired
  if (one_file) {
    write_fst(dts_format(dts, trim = trim), paste0(out_path, '_data.fst'), compress = 30)
    return()
  } else if (trim) {
    dts <- trim_dts(dts)
  }
  
  write_fst(rbindlist(map(dts, function(x) x$meta)), 
            paste0(out_path, 'meta.fst'),
            compress = 10)
  
  write_fst(rbindlist(map(dts, function(x) x$data)), 
            paste0(out_path, 'data.fst'),
            compress = 30)
  
}


#' trim_dts
#'
#' @param dts object from read_dts to be trimmed by the fibre length 
#'
#' @return
#' @export
#'
#' @examples
trim_dts <- function(dts) {

  wh <- which(dts[[1]]$data$distance %between% c(0, dts[[1]]$meta$fibre_length))

  dts <- map(dts, function(x){
    x$data <- x$data[wh]
    return(x)
  })
  
}


#' dts_format
#'
#' @param dts dts file 
#' @param trim output to exclude certain distances
#'
#' @return
#' @export
#'
#' @examples
dts_format <- function(dts, trim = TRUE) {
  

  rbindlist(map(dts, trim = trim, function(x, ...){
    if (trim) {
      x$data <- x$data[distance %between% c(0, x$meta$fibre_length)]
    }
    x$data[, `:=`(probe_1 = x$meta$probe_1,
                  probe_2 = x$meta$probe_2)]
    return(x$data)
  }))
  
}


# xml2 version /  currently is much slower
  # y <- read_xml('/media/kennel/Data/tmp/DTS_Test/channel 1_20150917203350375.xml', options = 'NOBLANKS')
  # ns <- xml_ns(y)
  # wh <- xml_find_all(y, xpath = '//d1:data', ns)
  # txt <- xml_text(wh)
  # tmp2 <- fread(input = paste(txt, collapse = '\n'))
