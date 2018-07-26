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

as.numeric(as.POSIXct('2017-05-17T17:26:27.375Z', format = "%Y-%m-%dT%H:%M:%OSZ", tz = 'UTC'))


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
      library(data.table) 
      library(anytime)})
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




# library(purrr)
# library(dts)
# library(foreach)
# library(doParallel)
# in_dir <- '/media/kennel/Data/tmp/DTS_Test'
# out_dir <- '/media/kennel/Data/tmp/'
# system.time(
#   b <- read_dts_folder(in_dir, n_cores = 4)
# )
# 
# 
# xml_files <- list.files(in_dir,
#                         pattern = '\\.xml$',
#                         full.names = TRUE)
# system.time(
#   a <- read_dts_folder('/media/kennel/Data/tmp/DTS_Test', 4)
# )
# 
# system.time({
# meta <- rbindlist(map(a, function(x) x$meta))
# data <- rbindlist(map(a, function(x) x$data))
# }
# )

# 
# system.time({
# 
# cl <- makeCluster(4)
# registerDoParallel(cl)
# 
# aa <- foreach(i = seq_along(xml_files), .export = 'read_dts_xml',  .packages=c('XML', 'data.table', 'anytime')) %dopar% {
#   read_dts_xml(xml_files[i])
# 
# }
# stopCluster(cl)
# })

# 
# test <- lapply(xml_files, read_dts_xml)

# 
# library(dts)
# system.time(
#   aa <- read_dts_folder('/media/kennel/Data/tmp/DTS_Test',
#                         '/media/kennel/Data/tmp/dts.fst', n_cores = 4)
# )
# 
# read_dts_xml(xml_files[703])
# read_dts_xml(xml_files[704])
# read_dts_xml(xml_files[705])
