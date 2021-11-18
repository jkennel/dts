#' get_distances
#'
#' @param file_path path to the file
#'
#' @return distances
#' @export
#'
#' @examples
get_distances <- function(xml_struct, type) {
  
  log_name        <- get_log_name_from_type(type)
  n_skip          <- get_n_skip_from_type(type)
  
  # get data
  # text <- paste(XML::getChildrenStrings(xml_struct[[log_name]][['logData']],
  #                                       addNames = FALSE,
  #                                       asVector = TRUE),
  #               collapse = '\n')
  # 
  # 
  # text  <-   data.table::fread(input = text,
  #                              blank.lines.skip = TRUE,
  #                              skip = n_skip,
  #                              select = 1,
  #                              sep = ',',
  #                              nThread = 1,
  #                              strip.white = FALSE)[[1]]
  
  
  # is cable double ended
  double_ended <- as.integer(XML::getChildrenStrings(xml_struct[[log_name]][['customData']][['isDoubleEnded']]))
  
  
  # length step increment
  if (type == 'ultima') {
    step_increment <- as.numeric(XML::getChildrenStrings(xml_struct[[log_name]][['blockInfo']][['stepIncrement']]))
  } else if (type == 'xt') {
    step_increment <- as.numeric(XML::getChildrenStrings(xml_struct[[log_name]][['stepIncrement']]))
  }

  # length of fiber
  fibre_length <- as.numeric(XML::getChildrenStrings(xml_struct[[log_name]][['customData']][['UserConfiguration']][['ChannelConfiguration']][['AcquisitionConfiguration']][['MeasurementLength']]))
  
  # subsets
  # wh <- which(text %between% c(0, fibre_length))
  
  return(list(
    # distances = text, 
    # wh = wh,
    double_ended = double_ended, 
    fibre_length = fibre_length,
    step_increment = step_increment))
}



