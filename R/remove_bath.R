#' get_bath_limits
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
get_bath_limits <- function(x) {
  
  mid_distance <- mean(x$trace_distance$distance)
  
  bath <- x$trace_distance[bath==TRUE]$distance
  down_cutoff <- max(bath[bath < mid_distance])
  up_cutoff   <- min(bath[bath > mid_distance])
  
  return(c(down_cutoff, up_cutoff))
  
}