#' set_zero_distance
#'
#' @param x 
#' @param zero_distance 
#'
#' @return
#' @export
#'
#' @examples
set_zero_distance <- function(x, zero_distance = NULL) {
  
  if(!is.null(zero_distance)) {
    x$trace_distance[, distance := distance - zero_distance] 
    x$trace_data[, distance := distance - zero_distance] 
  } else {
    
    coldspray <- min(subset_distance(get_distance_table(x), by = 'borehole')$distance)
    
    x$trace_distance[, distance := distance - coldspray] 
    x$trace_data[, distance := distance - coldspray] 
  
  }
  
  x
}