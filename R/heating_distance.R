#' heating_distance
#'
#' @param x 
#' @param n number of points after heating starts
#' @param temperature_change temperature change required to include.  
#' @param heating_type 
#'
#' @return
#' @export
#'
#' @examples
heating_distance <- function(x,
                             n = 10, 
                             temperature_change = 0.5,
                             power = 5,
                             heating_type = 'both') {
  
  cor_sum <- 0
  cor_cutoff <- 0
  
  if(n < 2) {
    stop('n must be at least 2')
  }
  if(heating_type == 'heating' | heating_type == 'both') {
    x_sub <- subset_distance(x, by = 'wh')
    heat  <- x_sub$trace_time[type == 'heating'][seq_len(n)]
    heat  <- x_sub$trace_data[heat[, list(start)], on = 'start']
    heat  <- to_matrix(heat)
    cor_sum <- cor_by_trace(heat, 1)^power
    diff_sum <- (diff_by_trace(heat, shift = floor(n/2), dim = 1))[, floor(n/2) + 1, drop = TRUE]
    wh <- which(diff_sum < temperature_change)
    cor_sum[wh] <- 0
    cor_cutoff <- 1
  }
  
  if (heating_type == 'cooling' | heating_type == 'both') {
    x_sub <- subset_distance(x, begin = 0, end = 1e5)
    cool <- x_sub$trace_time[type == 'cooling'][seq_len(n)]
    cool <- x_sub$trace_data[cool[, list(start)], on = 'start']
    cool <- to_matrix(cool)
    cor_sum <- cor_sum + cor_by_trace(cool, 1)^power
    cor_cutoff <- cor_cutoff + 1
 
  }
  
  
  # find junction distances
  n       <- length(cor_sum)
  
  # find heated distances
  
  is_heated  <- range(which(cor_sum > (cor_cutoff * 0.96)))
  heat_rng <- x_sub$trace_distance[is_heated]$distance
  x$trace_distance[between(distance, heat_rng[1], heat_rng[2]), heated := TRUE]

  junc    <- n/2-10 + which.min(cor_sum[(n/2 - 10):(n/2 + 10)])
  x$trace_distance[junc, junction := TRUE]
  
  x
  
}
