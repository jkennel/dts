#' heating_time
#'
#' @param x 
#' @param n 
#' @param heating_type
#'
#' @return
#' @export
#'
#' @examples
heating_time <- function(x, 
                         n = -10, 
                         heating_type = 'both', 
                         distance_range = NULL) {
  
  x$trace_time[, type := 'ambient']
  
  if(!is.null(distance_range)) {
    mat <- to_matrix(subset_distance(x,
                                     begin = distance_range[1], 
                                     end = distance_range[2]))
  } else {
    mat <- to_matrix(x)
  }
  
  bp <- time_breakpoints(mat, shift = n, type = heating_type)
  
  x$trace_time[between(start, bp[1], bp[2]), type := 'heating']
  x$trace_time[start >= bp[2], type := 'cooling']
  
  x$trace_time[, elapsed_time := as.numeric(start) - as.numeric(start[1]), by = type]
  
  x
  
}
