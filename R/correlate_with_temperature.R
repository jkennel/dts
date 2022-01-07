#' correlate_with_temperature
#'
#' @param x 
#' @param calib_t 
#' @param buffer 
#' @param power 
#' @param n_rem 
#'
#' @return
#' @export
#'
#' @examples
correlate_with_temperature <- function(x, 
                                       calib_t, 
                                       buffer = 0.1, 
                                       power = 8,
                                       n_rem = 3) {
  
  
  temp    <- to_matrix(x)
  r_2     <- cor_with_trace(temp, calib_t, 1)^power

  
  # determine cutoff
  n       <- length(r_2)
  max_r_2 <- max(r_2)
  cutoff  <- max(max_r_2) - buffer * max_r_2
  wh      <- which(r_2 > cutoff)
  wh_1    <- wh[wh <= n/2]
  wh_2    <- wh[wh > n/2]
  
  
  # remove first and last n_rem values
  wh      <- c(wh_1[-c(1:n_rem, length(wh_1):(length(wh_1)+1-n_rem))],
               wh_2[-c(1:n_rem, length(wh_2):(length(wh_2)+1-n_rem))])
  
  wh
}
