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
  
  # get times
  start <- anytime(getChildrenStrings(r[['wellLog']][['minDateTimeIndex']]))
  end   <- anytime(getChildrenStrings(r[['wellLog']][['maxDateTimeIndex']]))
  
  # get reference temperatures
  probe_1  <- as.numeric(getChildrenStrings(r[['wellLog']][['customData']][['probe1Temperature']]))
  probe_2  <- as.numeric(getChildrenStrings(r[['wellLog']][['customData']][['probe2Temperature']]))
  
  # does not appear to be used
  # ref_temp <- as.numeric(getChildrenStrings(r[['wellLog']][['customData']][['referenceTemperature']]))
  
  # max fibre length
  fibre_length <- as.numeric(getChildrenStrings(r[['wellLog']][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']]))
  
  
  # get data
  text <- paste(getChildrenStrings(r[['wellLog']][['logData']], 
                                   addNames = FALSE,
                                   asVector = TRUE),
                collapse = '\n')
  dat  <- fread(text, blank.lines.skip = TRUE)
  wide <- ncol(dat) > 4
  
  # check whether which format
  if(wide) {
    dat  <- dat[, c(1, 6), with = FALSE] 
  } else {
    dat  <- dat[, c(1, 4), with = FALSE] 
  }
  setnames(dat, c('distance', 'temp'))
  
  dat[, datetime := start]
  if (probe_1 < 200) {
    dat[, temp_ref_1 := probe_1]
  }
  if (probe_2 < 200) {
    dat[, temp_ref_2 := probe_2]
  }
  dat <- dat[distance < fibre_length]
  
  return(dat)
}

#' read_dts_folder
#'
#' @param in_dir location of the xml files
#' @param out_dir location of the xml files
#' @param ... arguments to pass to mclapply
#'
#' @return
#' @export
#'
#' @examples
read_dts_folder <- function(in_dir, out_dir, ...) {
  xml_files <- list.files(in_dir, 
                          pattern = '\\.xml$', 
                          full.names = TRUE)
  write_fst(rbindlist(mclapply(xml_files, read_dts_xml, ...)), out_dir, compress = 50)
}

# library(dts)
# system.time(
#   aa <- read_dts_folder('/media/kennel/Data/tmp/dts/',
#                         '/media/kennel/Data/tmp/dts.fst', mc.cores = 4)
# )