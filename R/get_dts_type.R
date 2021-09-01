#' get_dts_type
#'
#' @param xml_struct 
#'
#' @return
#' @export
#'
#' @examples
get_dts_type <- function(xml_struct) {
  
  log_name <- names(xml_struct[1])
  
  if(log_name == 'log') {
    return('xt')
  } else if(log_name == 'wellLog'){
    return('ultima')
  } else {
    stop('dts type is unknown')
  }
  
  
}


#' get_start_time_from_type
#'
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
get_start_time_from_type <- function(type){
  if (type == 'xt') {
    return('startDateTimeIndex')
  } else if (type == 'ultima'){
    return('minDateTimeIndex')
  }
} 

#' get_end_time_from_type
#'
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
get_end_time_from_type <- function(type){
  if (type == 'xt') {
    return('endDateTimeIndex')
  } else if (type == 'ultima'){
    return('maxDateTimeIndex')
  }
} 

#' get_log_name_from_type
#'
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
get_log_name_from_type <- function(type){
  if (type == 'xt') {
    return('log')
  } else if (type == 'ultima'){
    return('wellLog')
  }
} 


#' get_n_skip_from_type
#'
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
get_n_skip_from_type <- function(type){
  if (type == 'xt') {
    return(2)
  } else if (type == 'ultima'){
    return(0)
  }
} 


