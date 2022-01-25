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
set_values_for_distance <- function(x,
                                    variable_name, 
                                    begin, 
                                    end, 
                                    specific_depths = NA_real_,
                                    set_to = TRUE) {
  
  if(!variable_name %in% names(x$trace_distance)) {
    x$trace_distance[, (variable_name) := NA]
  }
  
  if (!is.na(specific_depths)) {
    x$trace_distance[distance %in% specific_depths,
                     (variable_name) := set_to]
  } else {
    
    x$trace_distance[between(distance, begin, end),
                     (variable_name) := set_to]
  }
  
  invisible(x)
}
