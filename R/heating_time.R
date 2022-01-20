#' heating_time
#'
#' @param x the DTS dataset
#' @param n the spacing between samples in time to calculate the difference.
#'  Should be a negative number.
#' @param heating_type the type of test.  Can be "heating" or "both".
#'
#' @return
#' @export
#'
#' @examples
heating_time <- function(x, 
                         n = -1, 
                         heating_type = 'both', 
                         distance_range = NULL) {
  
  x_sub <- subset_distance(x, by = 'wh')
  
  x_sub$trace_time[, type := 'ambient']
  
  if(!is.null(distance_range)) {
    mat <- to_matrix(subset_distance(x_sub,
                                     begin = distance_range[1], 
                                     end = distance_range[2]))
  } else {
    mat <- to_matrix(x_sub)
  }
  
  bp <- time_breakpoints(mat,
                         shift = n, 
                         type = heating_type)
  
  x$trace_time[between(start, bp[1], bp[2]), type := 'heating']
  x$trace_time[start >= bp[2], type := 'cooling']
  
  x$trace_time[, elapsed_time := as.numeric(start) - as.numeric(start[1]), by = type]
  
  x
  
}
