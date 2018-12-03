#' Convert R datetime to matlab datetime
#'
#' @param tms the POSIXct datetime in R
#'
#' @return datetime in matlab format
#' @export
#'
#' @examples
#' tms <- as.POSIXct('2016-01-01 12:12:10', tz = 'UTC')
#' dates_r_to_matlab(tms)
#'  
#'
#' as.POSIXct((dates_r_to_matlab(tms) - 719529) * 86400, 
#' origin = "1970-01-01", tz = "UTC")
dates_r_to_matlab <- function(tms) {
  
  origin <- as.POSIXct('0000-01-01', tz = 'UTC')
  
  return(((as.numeric(tms)-as.numeric(origin)) / 86400.0) + 1.0)
  
}



#' read_dts_xml
#'
#' @param in_dir path to the input folder
#' @param n_cores number of cores to use for parallel processing
#' @param date_format 'matlab' or 'R' format
#'
#' @return dts list of results
#' @export
#'
#'
read_dts_xml <- function(in_dir, n_cores, date_format = 'R') {
  
  # get DTS files
  fn <- list.files(in_dir, full.names = TRUE, pattern = "\\.xml$")
  
  event_info <- get_distances(fn[1])
  
  cl <- makePSOCKcluster(n_cores)
  
  dts <- parLapply(cl, fn, function(x){
    
    # Parse XML file
    r <- XML::xmlRoot(XML::xmlParse(x))
    
    # Get datetime of measurement
    start <- as.POSIXct(XML::getChildrenStrings(r[['wellLog']][['minDateTimeIndex']]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
    end   <- as.POSIXct(XML::getChildrenStrings(r[['wellLog']][['maxDateTimeIndex']]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
    
    
    # convert to matlab datetime
    if (date_format == 'matlab') {
      start <- dates_r_to_matlab(start)
      end <- dates_r_to_matlab(end)
    }
    
    
    # Is cable double ended 
    double_ended <- as.integer(XML::getChildrenStrings(r[['wellLog']][['customData']][['isDoubleEnded']]))
    
    if(double_ended){
      select <- c(6)
    } else {
      select <- c(4)
    }
    
    # Get reference temperatures
    probe_1  <- as.numeric(XML::getChildrenStrings(r[['wellLog']][['customData']][['probe1Temperature']]))
    probe_2  <- as.numeric(XML::getChildrenStrings(r[['wellLog']][['customData']][['probe2Temperature']]))
    
    
    # Get data
    text <- paste(XML::getChildrenStrings(r[['wellLog']][['logData']], 
                                          addNames = FALSE,
                                          asVector = TRUE),
                  collapse = '\n')
    
    text <- data.table::fread(input = text, 
                              blank.lines.skip = TRUE, 
                              select = select, 
                              sep = ',', 
                              nThread = 1, 
                              strip.white = FALSE)[[1]]
    
    
    # get metadata
    meta <- data.table::data.table(start, end, probe_1, probe_2)
    
    return(list(trace_data = text, trace_info = meta))
  })
  
  # top cluster
  stopCluster(cl)
  
  
  dts <- list(event_info = event_info, dts = dts)
}



#' dts_to_long
#'
#' @param dts list read from read_dts_xml
#'
#' @return list of data.tables
#' @export
#'
#' @examples
dts_to_long <- function(dts) {
  
  distance <- dts$event_info$distances
  wh <- dts$event_info$wh
  
  dts_long <- rbindlist(map(dts$dts, .f = function(x, ...) {
    data.table(temperature = x$trace_data, 
               datetime = x$trace_info$start,
               distance = distance)[wh]
  }, wh = wh))
  
  
  meta <- rbindlist(map(dts$dts, function(x) {
    x$trace_info
  }))
  
  return(list(trace_data = dts_long, trace_info = meta))
}



#' dts_to_wide
#'
#' @param dts list read from read_dts_xml
#'
#' @return matrix of dts results
#' @export
#'
#' @examples
dts_to_wide <- function(dts) {
  
  wh <- dts$event_info$wh
  distance <- dts$event_info$distances[wh]
  
  times <- c(0.0, as.numeric(map(dts$dts, function(x){
    x$trace_info$start
  })))
  
  probe_1 <- c(0.0, as.numeric(map(dts$dts, function(x){
    x$trace_info$probe_1
  })))
  
  probe_2 <- c(0.0, as.numeric(map(dts$dts, function(x){
    x$trace_info$probe_2
  })))
  
  sec <- times - times[2]
  hr <- sec / 3600
  
  # set first values to zero
  sec[1] <- 0.0
  hr[1]  <- 0.0
  
  dts_wide <- t(do.call('rbind', map(dts$dts, function(x){
    x$trace_data
  }))[, wh])
  
  dts_wide <- cbind(distance, dts_wide)
  dts_wide <- rbind(times, sec, hr, probe_1, probe_2, dts_wide)
  
}



#' read_dts_xml
#'
#' @param in_dir path to the input folder
#' @param out_name path to the output file
#' @param n_cores number of cores to use for parallel processing
#'
#' @return dts list of results
#' @export
#'
#' @examples
dts_to_matlab <- function(in_dir, out_name, n_cores = 4) {
  

  
  rmatio::write.mat(list(dts = dts_to_wide(read_dts_xml(in_dir, n_cores, 'matlab'))), 
                    out_name, 
                    compression = TRUE, 
                    version = 'MAT5')
}

#' get_distances
#'
#' @param file_path path to the file
#'
#' @return distances
#' @export
#'
#' @examples
get_distances <- function(file_path) {
  
  r <- XML::xmlRoot(XML::xmlParse(file_path))
  
  # get data
  text <- paste(XML::getChildrenStrings(r[['wellLog']][['logData']],
                                        addNames = FALSE,
                                        asVector = TRUE),
                collapse = '\n')
  
  
  text  <-   data.table::fread(input = text,
                               blank.lines.skip = TRUE,
                               select = 1,
                               sep = ',',
                               nThread = 1,
                               strip.white = FALSE)[[1]]
  
  
  # is cable double ended
  double_ended <- as.integer(XML::getChildrenStrings(r[['wellLog']][['customData']][['isDoubleEnded']]))
  
  # is cable double ended
  fibre_length <- as.numeric(XML::getChildrenStrings(r[['wellLog']][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']]))
  
  # subsets
  wh <- which(text %between% c(0, fibre_length))
  
  return(list(distances = text, wh = wh, double_ended = double_ended, fibre_length = fibre_length))
}


#' #' read_dts_xml
#' #'
#' #' @param file_path path to the file
#' #' @param double_ended is the cable double ended
#' #' @param wh subset
#' #'
#' #' @return dts table of results
#' #' @export
#' #'
#' #' @examples
#' read_dts_xml <- function(file_path, double_ended, wh) {
#' 
#'   r <- XML::xmlRoot(XML::xmlParse(file_path))
#'   
#'   return(list(meta = read_dts_xml_meta(r), 
#'               data = read_dts_xml_data(r, double_ended, wh)))
#' }
#' 
#' #' read_dts_xml_meta
#' #'
#' #' @param r root node of xml file
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' read_dts_xml_meta <- function(r) {
#'   
#'   # get times
#'   start <- as.POSIXct(XML::getChildrenStrings(r[['wellLog']][['minDateTimeIndex']]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
#'   end   <- as.POSIXct(XML::getChildrenStrings(r[['wellLog']][['maxDateTimeIndex']]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
#'   
#'   # is cable double ended 
#'   #double_ended <- as.integer(XML::getChildrenStrings(r[['wellLog']][['customData']][['isDoubleEnded']]))
#'   
#'   # get reference temperatures
#'   probe_1  <- as.numeric(XML::getChildrenStrings(r[['wellLog']][['customData']][['probe1Temperature']]))
#'   probe_2  <- as.numeric(XML::getChildrenStrings(r[['wellLog']][['customData']][['probe2Temperature']]))
#'   
#'   # max fibre length
#'   #fibre_length <- as.numeric(XML::getChildrenStrings(r[['wellLog']][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']]))
#'   
#'   return(data.table::data.table(start, end, probe_1, probe_2))
#'   
#' }
#' 
#' #' read_dts_xml_data
#' #'
#' #' @param r root node of xml file
#' #' @param double_ended root is the cable double ended
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' read_dts_xml_data <- function(r, double_ended) {
#'   
#'   # get times
#'   start <- as.POSIXct(XML::getChildrenStrings(r[['wellLog']][['minDateTimeIndex']]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
#'   
#'   # max fibre length
#'   #fibre_length <- as.numeric(XML::getChildrenStrings(r[['wellLog']][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']]))
#'   
#'   # is cable double ended 
#'   #double_ended <- as.integer(XML::getChildrenStrings(r[['wellLog']][['customData']][['isDoubleEnded']]))
#'   if (double_ended) {
#'     select <- c(6)
#'   } else {
#'     select <- c(4)
#'   }
#'   
#'   # get data
#'   text <- paste(XML::getChildrenStrings(r[['wellLog']][['logData']], 
#'                                    addNames = FALSE,
#'                                    asVector = TRUE),
#'                 collapse = '\n')
#'   
#'   
#'   text  <-   data.table::fread(input = text, 
#'                                blank.lines.skip = TRUE, 
#'                                select = select, 
#'                                sep = ',', 
#'                                nThread = 1, 
#'                                strip.white = FALSE)
#'   
#'   #setnames(text, c('distance', 'temperature'))
#'   data.table::setnames(text, c('temperature'))
#'   
#'   return(text)
#'   
#' }
#' 
#'   
#' #' read_dts
#' #'
#' #' @param in_path location of the xml files
#' #' @param n_cores specify the number of cores
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' read_dts <- function(in_path, n_cores = 1) {
#' 
#'   # read single file
#'   if(file_test('-f', in_path)) {
#'     return(lapply(in_path, read_dts_xml))
#'   }
#'   
#'   # read directory
#'   xml_files <- list.files(in_path, 
#'                           pattern = '\\.xml$', 
#'                           full.names = TRUE)
#'   
#'   dists <- get_distances(xml_files[1])
#'   double_ended <- dists[['double_ended']]
#'   
#'   if(n_cores == 1) {
#'     return(lapply(xml_files, read_dts_xml, double_ended))
#'   } else {
#'     # Initiate cluster
#'     cl <- makePSOCKcluster(n_cores)
#'     
#'     clusterExport(cl = cl, list('double_ended', 
#'                                 'read_dts_xml', 
#'                                 'read_dts_xml_data', 
#'                                 'read_dts_xml_meta'))
#'     
#'     res <- parallel::parLapply(cl, xml_files, read_dts_xml, double_ended = double_ended)
#'     
#'     stopCluster(cl)
#'   }
#'   
#'   return(res)
#' }
#' 
#' 
#' #' write_dts
#' #'
#' #' @param in_path input directory
#' #' @param out_path output directory
#' #' @param n_cores how many cores
#' #' @param one_file output a single file
#' #' @param trim output to exclude certain distances
#' #' 
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' write_dts <- function(in_path, out_path, n_cores = 1, 
#'                       one_file = TRUE, trim = TRUE) {
#' 
#'   dts  <- read_dts(in_path, n_cores)
#' 
#'   # if one file is desired
#'   if (one_file) {
#'     write_fst(dts_format(dts, trim = trim), paste0(out_path, '_data.fst'), compress = 30)
#'     return()
#'   } else if (trim) {
#'     dts <- trim_dts(dts)
#'   }
#'   
#'   write_fst(rbindlist(map(dts, function(x) x$meta)), 
#'             paste0(out_path, 'meta.fst'),
#'             compress = 10)
#'   
#'   write_fst(rbindlist(map(dts, function(x) x$data)), 
#'             paste0(out_path, 'data.fst'),
#'             compress = 30)
#'   
#' }
#' 
#' 
#' #' trim_dts
#' #'
#' #' @param dts object from read_dts to be trimmed by the fibre length 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' trim_dts <- function(dts) {
#' 
#'   wh <- which(dts[[1]]$data$distance %between% c(0, dts[[1]]$meta$fibre_length))
#' 
#'   dts <- map(dts, function(x){
#'     x$data <- x$data[wh]
#'     return(x)
#'   })
#'   
#' }
#' 
#' 
#' #' dts_format
#' #'
#' #' @param dts dts file 
#' #' @param trim output to exclude certain distances
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' dts_format <- function(dts, trim = TRUE) {
#'   
#' 
#'   rbindlist(map(dts, trim = trim, function(x, ...){
#'     if (trim) {
#'       x$data <- x$data[distance %between% c(0, x$meta$fibre_length)]
#'     }
#'     x$data[, `:=`(probe_1 = x$meta$probe_1,
#'                   probe_2 = x$meta$probe_2)]
#'     return(x$data)
#'   }))
#'   
#' }


# xml2 version /  currently is much slower
  # y <- read_xml('/media/kennel/Data/tmp/DTS_Test/channel 1_20150917203350375.xml', options = 'NOBLANKS')
  # ns <- xml_ns(y)
  # wh <- xml_find_all(y, xpath = '//d1:data', ns)
  # txt <- xml_text(wh)
  # tmp2 <- fread(input = paste(txt, collapse = '\n'))
