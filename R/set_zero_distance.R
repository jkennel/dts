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
  
  if(is.null(zero_distance)) {
    zero_distance <- subset_distance(x, by = 'borehole')
    zero_distance <- min(get_distance_table(zero_distance)$distance)
  }
  x$trace_distance[, distance := distance - zero_distance] 
  x$trace_data[, distance := distance - zero_distance] 
  
  x
}