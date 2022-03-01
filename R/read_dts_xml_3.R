#' read_one_xml
#'
#' @param fn 
#'
#' @return
#' @export
#'
#' @examples
read_one_xml <- function(fn) {
  
  z <- XML::xmlParse(fn)
  r <- XML::xmlRoot(z)
  
  
  # get DTS equipment type
  type <- get_dts_type(r)
  
  
  # equipment specific XML fields
  start_time_name <- get_start_time_from_type(type)
  end_time_name   <- get_end_time_from_type(type)
  log_name        <- get_log_name_from_type(type)
  n_skip          <- get_n_skip_from_type(type)
  
  
  # Is cable double ended 
  double_ended <- as.integer(XML::getChildrenStrings(
    r[[log_name]][['customData']][['isDoubleEnded']], 
    len = 1L, 
    addNames = FALSE))
  
  
  # length step increment
  if (type == 'ultima') {
    
    step_increment <- as.numeric(XML::getChildrenStrings(
      r[[log_name]][['blockInfo']][['stepIncrement']],
      len = 1L, 
      addNames = FALSE))
    
  } else if (type == 'xt') {
    
    step_increment <- as.numeric(XML::getChildrenStrings(
      r[[log_name]][['stepIncrement']],
      len = 1L,
      addNames = FALSE))
    
  }
  
  # length of fiber
  fibre_length <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']],
    len = 1L, addNames = FALSE))
  
  
  
  # get a few constants from the setup
  differential_loss_internal <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['SystemSettings']][['MeasurementSettings']][['InternalDifferentialLoss']],
    len = 1L, 
    addNames = FALSE))
  temperature_scaling <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['SystemSettings']][['MeasurementSettings']][['TemperatureScalingFactor']],
    len = 1L, 
    addNames = FALSE))
  averaging_time <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['SystemSettings']][['MeasurementSettings']][['InternalAveragingTime']],
    len = 1L, 
    addNames = FALSE))
  
  effective_stokes <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['SystemSettings']][['RawProcessingSettings']][['EffectiveStokesRI']],
    len = 1L,
    addNames = FALSE))
  effective_anti_stokes <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['SystemSettings']][['RawProcessingSettings']][['EffectiveAntiStokesRI']],
    len = 1L, 
    addNames = FALSE))
  
  correct_for_zig_zag <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['SystemSettings']][['RawProcessingSettings']][['CorrectForZigZag']],
    len = 1L,
    addNames = FALSE))
  laser_on_length <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['SystemSettings']][['RawProcessingSettings']][['LaserOnLength']],
    len = 1L,
    addNames = FALSE))
  
  
  ref_temp_start <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['SystemSettings']][['TemperatureReferenceSettings']][['InternalReferenceStart']],
    len = 1L,
    addNames = FALSE))
  ref_temp_end <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['SystemSettings']][['TemperatureReferenceSettings']][['InternalReferenceStop']],
    len = 1L, 
    addNames = FALSE))
  
  configuration_name <- XML::getChildrenStrings(
    r[[log_name]][['customData']][['UserConfiguration']][['MainMeasurementConfiguration']][['ConfigurationName']],
    len = 1L,
    addNames = FALSE)
  measurement_interval <- as.numeric(XML::getChildrenStrings(
    r[[log_name]][['customData']][['UserConfiguration']][['MainMeasurementConfiguration']][['MeasurementInterval']],
    len = 1L,
    addNames = FALSE))
  # laser_frequency <- as.numeric(XML::getChildrenStrings(r[[log_name]][['customData']][['UserConfiguration']][['MainMeasurementConfiguration']][['LaserFrequency']]))
  
  
  # get channel info
  channels <- XML::xmlChildren(r[[log_name]][['customData']][['SystemSettings']])
  channels <- channels[names(channels) == 'ChannelSettings']
  channels <- data.table::rbindlist(lapply(channels, function(x) {
    y <- XML::getChildrenStrings(x)
    data.table::data.table(channel_number = as.numeric(y[1]),
                           internal_fiber_length = as.numeric(y[2]))
  }))
  
  channels2 <- XML::xmlChildren(r[[log_name]][['customData']][['UserConfiguration']])
  channels2 <- channels2[names(channels2) == 'ChannelConfiguration']
  channels2 <- data.table::rbindlist(lapply(channels2, function(x) {
    data.table::data.table(
      
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
  
  
  text <- paste0(XML::getChildrenStrings(r[[log_name]][['logData']],
                                         addNames = FALSE,
                                         asVector = TRUE),
                 collapse = '\n')
  
  if(type == 'xt') {
    skip <- 1
  } else {
    skip <- 0
  }
  text <- data.table::fread(input = text,
                            blank.lines.skip = TRUE,
                            select = 1,
                            sep = ',',
                            nThread = 1,
                            skip = skip,
                            strip.white = FALSE)[[1]]
  
  XML::free(z)
  rm(z)
  rm(r)
  
  # return data.table with values
  device <- data.table::data.table(
    type,
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
    ref_temp_end,
    fibre_length,
    double_ended,
    step_increment
  )
  
  return(list(channels = channels,
              device = device,
              distance = text))
}


#' read_dts_xml_3
#'
#' @param in_dir 
#' @param out_dir 
#' @param n_cores 
#' @param max_files 
#' @param return_stokes 
#' @param in_memory 
#' @param output_rds 
#' @param trim 
#' @param time_aggregate_interval 
#'
#' @return
#' @export
#'
#' @examples
read_dts_xml_3 <- function(in_dir, 
                           out_dir = getwd(),
                           n_cores = 1, 
                           max_files = Inf,
                           return_stokes = FALSE, 
                           in_memory = FALSE,
                           output_rds = FALSE,
                           trim = TRUE,
                           time_aggregate_interval = 60L) {
  
  
  # get, subset, and sort xml file names
  fn <- list.files(in_dir, 
                   full.names = FALSE, 
                   pattern = '*.xml$')
  
  fn <- sort(fn, method = 'radix')[1:pmin(length(fn), max_files)]
  
  fn <- file.path(in_dir, fn)
  
  
  # get the constant meta data from the first XML file  
  meta <- read_one_xml(fn[1])
  
  
  type <- meta$device$type
  if(type == 'xt') {
    data_pattern <- c('<data>\n', '</data>\n')
  } else {
    data_pattern <- c('<data uid="measurement">', '</data>')
  }
  
  
  # set column names
  double_ended <- meta[['device']][['double_ended']]
  if(double_ended){
    if(return_stokes) {
      select <- c(1L:6L)
    } else {
      select <- c(1L, 6L)
    }
    nms <- c('distance', 'stokes', 'anti_stokes', 'rev_stokes', 'rev_anti_stokes', 'temperature')[select]
  } else {
    if(return_stokes) {
      select <- c(1L:4L)
    } else {
      select <- c(1L, 4L)
    }
    nms <- c('distance', 'stokes','anti_stokes', 'temperature')[select]
  }
  
  
  # strings to find in xml
  keys <- xml_key(type)
  
  # path for the output
  folder_path <- file.path(out_dir, 'dts_data')
  
  if(!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  
  # overwrite existing files (the csv file is treated as disposable)
  out_file <- file.path(folder_path, 'dts_data.csv')
  unlink(out_file)
  
  # write the new file with header
  fwrite(as.list(c(nms, 'start')), out_file)
  
  # set up parallel cluster
  cl <- parallel::makePSOCKcluster(n_cores)
  parallel::clusterExport(cl = cl, 
                          varlist = c(
                            'folder_path',
                            'select',
                            'type',
                            'keys',
                            'data_pattern'), 
                          envir = environment())
  
  dts <- parallel::parLapply(cl, fn, function(x) {
    
    
    # Fast read 
    xml_text <- dts::read_file_cpp(x)
    
    # Find the start and end of the data
    if(type == 'xt') {
      s <- regexpr('<data>', xml_text, fixed = TRUE)[[1]][1] + 6L
      e <- regexpr('</logData>', xml_text, fixed = TRUE)[[1]][1] - 1L
      bot <- substr(xml_text, e, e + 900L)
      
      vals <- as.list(c(
        as.numeric(fastPOSIXct(stri_match_first_regex(xml_text, keys$pattern[1:2])[, 2])),
        as.numeric(stri_match_first_regex(bot, keys$pattern[3:11])[, 2])))
      
    } else {
      s <- regexpr('<logData>', xml_text, fixed = TRUE)[[1]][1] + 9L
      e <- regexpr('</logData>', xml_text, fixed = TRUE)[[1]][1] - 1L
      
      vals <- stri_match_first_regex(xml_text, keys$pattern)[, 2]
      vals <- as.list(c(
        as.numeric(fastPOSIXct(vals[1:2])), 
        as.numeric(vals[3:11])))
    }
    
    
    # Read in the data
    dat <- fread(stri_replace_all_fixed(
      substr(xml_text, s, e),
      pattern = data_pattern,
      replacement = c(""),
      vectorize_all = FALSE),
      select = select, 
      colClasses = 'numeric',
      blank.lines.skip = TRUE,
      nThread = 1)
    
    # Add start time
    set(dat, j = 'start', value = vals[[1]])
    
    
    # Write data to file in append mode
    fwrite(dat,
           file = file.path(folder_path, 'dts_data.csv'),
           append = TRUE,
           quote = FALSE,
           col.names = FALSE,
           nThread = 1)
    
    setDT(vals)
  })
  
  # stop cluster
  parallel::stopCluster(cl)
  
  dts <- rbindlist(dts)
  setnames(dts, keys$names)
  
  set(dts, 
      j = 'calib_temperature', 
      value = (dts[['probe_1']] + dts[['probe_2']]) / 2.0)
  
  if (in_memory) {
    dat <- fread(file = out_file, 
                 col.names = c(nms, 'start'),
                 colClasses = 'numeric')
    
    
    dat[, start := anytime(start, tz = 'UTC')]
    setkey(dat, start, distance)
    
    dts[, mid := (start + end) / 2.0]
    dts[, start := anytime(start, tz = 'UTC')]
    dts[, mid := anytime(mid, tz = 'UTC')]
    dts[, end := anytime(end, tz = 'UTC')]
    
  } else {
    
    # Read in the on disk file
    # Aggregate based on a time subset interval
    #   - Give mean and standard deviation
    dat <- arrow::open_dataset(out_file,
                               format = 'csv') |>
      arrow::to_duckdb() |>
      dplyr::group_by(start = floor(start / time_aggregate_interval) *
                        time_aggregate_interval, distance) |>
      dplyr::summarise(temperature_sd = stddev_pop(temperature),
                       temperature = mean(temperature, na.rm = TRUE)) |>
      dplyr::collect() |>
      dplyr::ungroup() |>
      data.table::setDT()
    
    setkey(dat, start, distance)
    
    # data.table aggregation method
    dts <- dts[, lapply(.SD, mean, na.rm = TRUE),
               by = list(start = anytime(
                 as.integer(start %/% time_aggregate_interval) *
                   time_aggregate_interval, tz = 'UTC'))]
    
    dts[, mid := start + time_aggregate_interval / 2.0]
    dts[, end := start + time_aggregate_interval]
    
    dat[, start := anytime(start, tz = 'UTC')]
    
  }
  
  setkey(dts, start)
  
  # distance table
  distance <- data.table(
    distance = meta$distance,
    wh = meta$distance %between% c(0.0, meta[['device']][['fibre_length']]),
    junction = FALSE,
    heated = FALSE,
    bath = FALSE,
    reference = FALSE,
    borehole = FALSE)
  
  
  # return the results
  dts <- list(
    trace_data = dat, 
    trace_time = dts,
    trace_distance = distance,
    device = meta$device,
    channels = meta$channels,
    dir = out_dir)
  
  class(dts) <- c('dts_long', class(dts))
  
  # write an RDS file
  if (output_rds) {
    saveRDS(dts, file.path(folder_path, 'dts.rds'))
  }
  
  return(dts)
}




