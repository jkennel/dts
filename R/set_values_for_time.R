#' set_values_for_distance
#'
#' @param x 
#' @param variable_name 
#' @param start_distance 
#' @param end_distance 
#' @param specific_depths 
#' @param set_to 
#'
#' @return
#' @export
#'
set_values_for_time <- function(x,
                                variable_name, 
                                begin, 
                                end, 
                                specific_times = NA_real_,
                                set_to = TRUE) {
  
  if(!variable_name %in% names(x$trace_time)) {
    set(x$trace_time, 
        j = variable_name,
        values = NA)
  }
  
  if (!is.na(specific_depths)) {
    x$trace_time[distance %in% specific_depths,
                 (variable_name) := set_to]
  } else {
    
    x$trace_time[between(distance, begin, end),
                 (variable_name) := set_to]
  }
  
  invisible(x)
}
