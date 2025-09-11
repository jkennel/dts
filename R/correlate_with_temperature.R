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
  
  
  
  temp <- to_matrix(x)
  r_2     <- cor_with_trace(temp, calib_t, 1L)^power

  # determine cutoff
  n       <- length(r_2)
  max_r_2 <- max(r_2, na.rm = TRUE)
  cutoff  <- max(max_r_2, na.rm = TRUE) - buffer * max_r_2
  wh      <- which(r_2 > cutoff)
  wh_1    <- wh[wh <= n/2]
  wh_2    <- wh[wh > n/2]
  

  
  # remove first and last n_rem values
  wh      <- c(wh_1[-c(1:n_rem, length(wh_1):(length(wh_1)+1-n_rem))],
               wh_2[-c(1:n_rem, length(wh_2):(length(wh_2)+1-n_rem))])
  
  
  # check temperature difference because correlation isn't the perfect metric
  wh_d <- rowSums(abs(temp[wh, ] - calib_t))
  med_wh_d <- median(wh_d)
  wh <- wh[which(abs(wh_d - med_wh_d) <  (med_wh_d * buffer))]
  
  wh
}


#' compare_with_temperature
#'
#' @param x 
#' @param calib_t 
#' @param buffer temperature difference in degrees
#' @param power 
#' @param n_rem 
#'
#' @return
#' @export
#'
#' @examples
compare_with_temperature <- function(x, 
                                     calib_t, 
                                     buffer = 0.1, 
                                     n_rem = 3) {
  
  
  # compare temperatures
  temp <- to_matrix(x)
  print(str(calib_t)) 
  print(str(temp))
  temp_diff <- rowSums(abs(temp - calib_t)) / length(calib_t)
  wh   <- which(temp_diff < (min(temp_diff) + buffer))
  
  # determine cutoff
  n    <- length(temp_diff)
  # print(wh)
  wh_1 <- wh[wh <= n/2]
  wh_2 <- wh[wh > n/2]
  # print(wh_1)
  # print(wh_2)
  
  # remove first and last n_rem values
  wh   <- c(wh_1[-c(1:n_rem, length(wh_1):(length(wh_1)+1-n_rem))],
               wh_2[-c(1:n_rem, length(wh_2):(length(wh_2)+1-n_rem))])
  
  
  wh
}