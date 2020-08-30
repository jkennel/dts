#' heating_time
#'
#' @param x 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
heating_time <- function(x, n = -10) {
  
  bp <- time_breakpoints(to_matrix(x), shift = n)
  
  x$trace_time[, type := 'ambient']
  x$trace_time[between(start, bp[1], bp[2]), type := 'heating']
  x$trace_time[start >= bp[2], type := 'cooling']
  
  x$trace_time[, elapsed_time := as.numeric(start) - as.numeric(start[1]), by = type]
  
  x
  
}
