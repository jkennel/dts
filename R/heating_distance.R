#' heating_distance
#'
#' @param x 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
heating_distance <- function(x, n = 50) {
  
  # find junction distances
  
  heat <- x$trace_time[type == 'heating'][seq_len(n)]
  cool <- x$trace_time[type == 'cooling'][seq_len(n)]
  
  heat <- x$trace_data[heat[, list(start)], on = 'start']
  cool <- x$trace_data[cool[, list(start)], on = 'start']
  
  heat <- to_matrix(heat)
  cool <- to_matrix(cool)
  
  cor_sum <- cor_by_trace(heat, 1) + cor_by_trace(cool, 1)
  
  
  n       <- length(cor_sum)
  
  junc    <- n/2-10 + which.min(cor_sum[(n/2 - 10):(n/2 + 10)])
  x$trace_distance[junc, junction := TRUE]
  
  
  # find heated distances
  
  is_heated  <- range(which(cor_sum > 1.8))
  x$trace_distance[is_heated[1]:is_heated[2], heated := TRUE]
  
  x
  
}
