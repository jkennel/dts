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



