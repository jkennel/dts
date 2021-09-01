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
  
  return(((as.numeric(tms) - as.numeric(origin)) / 86400.0) + 1.0)
  
}


# dates_matlab_to_r <- function(tms) {
#   
#   origin <- as.POSIXct('0000-01-01', tz = 'UTC')
#   
#   return(((as.numeric(tms) - as.numeric(origin)) / 86400.0) + 1.0)
#   
# }


#' read_dts_zip
#'
#' @param file path to the zip
#' @param n_cores number of cores to use for parallel processing
#' @param date_format 'matlab' or 'R' format
#'
#' @return dts list of results
#' @export
#'
#' @example 
#' file_path <- "/home/jonathankennel/Storage/analyses/sentry_mitchell_dts/data/2020-10-29 SEN7 MLS A-DTS 15Wm 24h.zip"
read_dts_zip <- function(file_path, ...) {
  
  t_dir <- file.path(tempdir(), 'dts', round(as.numeric(Sys.time())))
  
  unzip(file_path, exdir = t_dir, junkpaths = TRUE)
  
  dts <- read_dts_xml(t_dir, ...)
  
  unlink(t_dir, recursive = TRUE)
  
  dts
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
  
  r <- XML::xmlRoot(XML::xmlParse(fn[1]))
 
  # get DTS equipment type
  type <- get_dts_type(r)

  
  # equipment specific XML fields
  start_time_name <- get_start_time_from_type(type)
  end_time_name   <- get_end_time_from_type(type)
  log_name        <- get_log_name_from_type(type)
  n_skip          <- get_n_skip_from_type(type)
  
  event_info <- get_distances(r, type)
  
  
  # get a few constants from the setup
  differential_loss_internal <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['SystemSettings']][['MeasurementSettings']][['InternalDifferentialLoss']]))
  temperature_scaling <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['SystemSettings']][['MeasurementSettings']][['TemperatureScalingFactor']]))
  averaging_time <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['SystemSettings']][['MeasurementSettings']][['InternalAveragingTime']]))

  effective_stokes <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['SystemSettings']][['RawProcessingSettings']][['EffectiveStokesRI']]))
  effective_anti_stokes <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['SystemSettings']][['RawProcessingSettings']][['EffectiveAntiStokesRI']]))
  correct_for_zig_zag <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['SystemSettings']][['RawProcessingSettings']][['CorrectForZigZag']]))
  laser_on_length <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['SystemSettings']][['RawProcessingSettings']][['LaserOnLength']]))


  ref_temp_start <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['SystemSettings']][['TemperatureReferenceSettings']][['InternalReferenceStart']]))
  ref_temp_end <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['SystemSettings']][['TemperatureReferenceSettings']][['InternalReferenceStop']]))

  configuration_name <- XML::getChildrenStrings(r[[log_name]][['customData']][['UserConfiguration']][['MainMeasurementConfiguration']][['ConfigurationName']])
  measurement_interval <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['UserConfiguration']][['MainMeasurementConfiguration']][['MeasurementInterval']]))
  # laser_frequency <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['UserConfiguration']][['MainMeasurementConfiguration']][['LaserFrequency']]))
  
  
  # get channel info
  channels <- XML::xmlChildren(r[[log_name]][['customData']][['SystemSettings']])
  channels <- channels[names(channels) == 'ChannelSettings']
  channels <- rbindlist(lapply(channels, function(x) {
    y <- XML::getChildrenStrings(x)
    data.table(channel_number = as.numeric(y[1]),
               internal_fiber_length = as.numeric(y[2]))
  }))
  
  channels2 <- XML::xmlChildren(r[[log_name]][['customData']][['UserConfiguration']])
  channels2 <- channels2[names(channels2) == 'ChannelConfiguration']
  channels2 <- rbindlist(lapply(channels2, function(x) {
    data.table(
      channel_number = as.numeric(XML::getChildrenStrings(x[['ChannelNumber']])),
      channel_name = XML::getChildrenStrings(x[['ChannelName']]),
      channel_is_active = XML::getChildrenStrings(x[['ChannelIsActive']]),
      
      sampling_interval = as.numeric(XML::getChildrenStrings(x[['AcquisitionConfiguration']][['SamplingInterval']])),
      measurement_length = as.numeric(XML::getChildrenStrings(x[['AcquisitionConfiguration']][['MeasurementLength']])),
      acquisition_time = as.numeric(XML::getChildrenStrings(x[['AcquisitionConfiguration']][['AcquisitionTime']])),
      
      differential_loss_calculation = as.numeric(XML::getChildrenStrings(x[['TemperatureCalibrationConfiguration']][['DifferentialLossConfiguration']][['DifferentialLossCalculation']])),
      differential_loss = as.numeric(XML::getChildrenStrings(x[['TemperatureCalibrationConfiguration']][['DifferentialLossConfiguration']][['DifferentialLoss']]))
      
    )
  }))
  channels <- channels[channels2, on ='channel_number']
  
  
  
  
  
  cl <- parallel::makePSOCKcluster(n_cores)
  parallel::clusterExport(cl=cl, varlist = c('start_time_name', 
                                   'end_time_name',
                                   'log_name',
                                   'n_skip',
                                   'date_format'), envir=environment())
  
  dts <- parallel::parLapply(cl, fn, function(x){

    # Parse XML file
    r <- XML::xmlRoot(XML::xmlParse(x))
    
    
    # Get datetime of measurement
    start <- fasttime::fastPOSIXct(XML::getChildrenStrings(r[[log_name]][[start_time_name]]), tz = 'UTC')
    end   <- fasttime::fastPOSIXct(XML::getChildrenStrings(r[[log_name]][[end_time_name]]), tz = 'UTC')
    mid   <- as.POSIXct(mean(as.numeric(c(start, end))), origin = '1970-01-01', tz = 'UTC')
    dt    <- as.numeric(end) - as.numeric(start)
    
    # convert to matlab datetime
    if (date_format == 'matlab') {
      start <- dates_r_to_matlab(start)
      end <- dates_r_to_matlab(end)
    }
    
    
    # Is cable double ended 
    double_ended <- as.integer(XML::getChildrenStrings(r[[log_name]][['customData']][['isDoubleEnded']]))
    
    if(double_ended){
      select <- c(2:6)
      nms <- c('stokes','anti_stokes', 'rev_stokes','rev_anti_stokes', 'temperature')
    } else {
      select <- c(2:4)
      nms <- c('stokes','anti_stokes', 'temperature')
    }
    
    # Get reference temperatures
    probe_1  <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['probe1Temperature']]))
    probe_2  <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['probe2Temperature']]))
    n_forward  <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['forwardSignalAverages']]))
    n_reverse  <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['reverseSignalAverages']]))
    
    ref_tmp  <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['referenceTemperature']]))
    ref_voltage  <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['referenceProbeVoltage']]))
    probe_1_voltage <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['probe1Voltage']]))
    probe_2_voltage <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['probe2Voltage']]))
    

    
    # Get data
    text <- paste0(XML::getChildrenStrings(r[[log_name]][['logData']],
                                          addNames = FALSE,
                                          asVector = TRUE),
                  collapse = '\n')
    text <- data.table::fread(input = text,
                              blank.lines.skip = TRUE,
                              select = select,
                              skip = n_skip,
                              colClasses = rep('numeric', 6),
                              sep = ',',
                              nThread = 1,
                              strip.white = FALSE)
    data.table::setnames(text, nms)
  
    
    # get metadata
    meta <- data.table::data.table(start, 
                                   end, 
                                   mid, 
                                   dt, 
                                   probe_1, 
                                   probe_2, 
                                   calib_temperature = (probe_1 + probe_2) / 2,
                                   ref_temperature = ref_tmp,
                                   ref_voltage,
                                   probe_1_voltage,
                                   probe_2_voltage,
                                   n_forward,
                                   n_reverse)
    
    
    return(list(trace_data = text, trace_time = meta))
  })
  
  # stop cluster
  stopCluster(cl)

  device <- data.table(
    configuration_name,
    measurement_interval,
    # laser_frequency,
    differential_loss_internal,
    temperature_scaling,
    averaging_time,
    effective_stokes,
    effective_anti_stokes,
    correct_for_zig_zag,
    laser_on_length,
    ref_temp_start,
    ref_temp_end
  )
  dts <- list(event_info = event_info,
              dts = dts, 
              device = device,
              channels = channels)
  
  class(dts) <- c('dts', class(dts))
  
  dts
}







#' dts_to_long
#'
#' @param dts list read from read_dts_xml
#' @param trim_distances trim distances
#'
#' @return list of data.tables
#' @export
#'
#' @examples
dts_to_long <- function(dts, trim_distances = TRUE) {
  
  distance <- dts[['event_info']][['distances']]
  
  if(trim_distances) {
    wh <- dts[['event_info']][['wh']]
  } else {
    wh <- 1:length(distance)
  }
  
  
  dts_long <- rbindlist(purrr::map(dts[['dts']], .f = function(x, ...) {

    data.table::data.table(
      x[['trace_data']], 
      start       = x[['trace_time']][['start']],
      distance = distance)[wh]
    
  }, wh = wh))
  
  
  meta <- rbindlist(purrr::map(dts[['dts']], function(x) {
    x[['trace_time']]
  }))
  meta[, type := NA_character_]
  
  cable_dist <- data.table(distance = distance[wh],
                     junction = FALSE,
                     heated = FALSE,
                     bath = FALSE,
                     reference = FALSE,
                     borehole = FALSE)
  
  setkey(meta, start)
  setkey(dts_long, start, distance)
  setkey(cable_dist, distance)
  
  dts <- list(trace_data = dts_long,
              trace_time = meta, 
              trace_distance = cable_dist,
              device = dts[['device']],
              channels = dts[['channels']])
  class(dts) <- c('dts_long', class(dts))
  
  dts
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
  
  wh <- dts[['event_info']][['wh']]
  distance <- dts[['event_info']][['distances']][['wh']]
  
  times <- c(0.0, as.numeric(purrr::map(dts[['dts']], function(x){
    x[['trace_time']][['start']]
  })))
  
  probe_1 <- c(0.0, as.numeric(purrr::map(dts[['dts']], function(x){
    x[['trace_time']][['probe_1']]
  })))
  
  probe_2 <- c(0.0, as.numeric(purrr::map(dts[['dts']], function(x){
    x[['trace_time']][['probe_2']]
  })))
  
  sec <- times - times[2]
  hr <- sec / 3600
  
  # set first values to zero
  sec[1] <- 0.0
  hr[1]  <- 0.0
  
  dts_wide <- t(do.call('rbind', purrr::map(dts[['dts']], function(x){
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
#'   start <- as.POSIXct(XML::getChildrenStrings(r[[log_name]][[start_time_name]]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
#'   end   <- as.POSIXct(XML::getChildrenStrings(r[[log_name]][[end_time_name]]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
#'   
#'   # is cable double ended 
#'   #double_ended <- as.integer(XML::getChildrenStrings(r[[log_name]][['customData']][['isDoubleEnded']]))
#'   
#'   # get reference temperatures
#'   probe_1  <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['probe1Temperature']]))
#'   probe_2  <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['probe2Temperature']]))
#'   
#'   # max fibre length
#'   #fibre_length <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']]))
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
#'   start <- as.POSIXct(XML::getChildrenStrings(r[[log_name]][[start_time_name]]), format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
#'   
#'   # max fibre length
#'   #fibre_length <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']]))
#'   
#'   # is cable double ended 
#'   #double_ended <- as.integer(XML::getChildrenStrings(r[[log_name]][['customData']][['isDoubleEnded']]))
#'   if (double_ended) {
#'     select <- c(6)
#'   } else {
#'     select <- c(4)
#'   }
#'   
#'   # get data
#'   text <- paste(XML::getChildrenStrings(r[[log_name]][['logData']], 
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


# library(microbenchmark)
# x <- "2020-02-02T20:00:30Z"
# microbenchmark(
#   a <- fastPOSIXct(x,  tz = 'UTC'),
#   b <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC')
# )

