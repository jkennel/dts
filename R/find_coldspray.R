#' find_coldspray
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
find_coldspray <- function(x, type = 'head') {
  
  
  mat <- to_matrix(x)
  
  # locate coldspray in time
  time_difference <- diff_by_trace(mat, shift = 2, dim = 1) # along time
  time_cols  <- which.min(apply(time_difference, 2, min))
  
  # find row with highest standard deviation
  pick_sd     <- which.max(apply(mat[, time_cols + -30:30], 1, sd))
  
  # find row with highest negative change
  pick_runmed <- which.min(mat[, time_cols] - runmed(mat[, time_cols], 7))
  
  
  if(pick_sd %in% (pick_runmed + -1:1)) {
    distance_cutoff <- mean(x$trace_distance$distance[c(pick_sd, pick_runmed)])
    
    if (type == 'head') {
      x$trace_distance[distance >= distance_cutoff, borehole := TRUE]
    } else if (type == 'tail'){
      x$trace_distance[distance <= distance_cutoff, borehole := TRUE]
    } else {
      max_distance <- max(x$trace_distance$distance, na.rm = TRUE)
      x$trace_distance[between(distance, distance_cutoff, max_distance-distance_cutoff), borehole := TRUE]
    }
    return(x)
    
  } else {
    warning('unable to automatically pick coldspray')
    x$trace_distance[, borehole := TRUE]
    return(x)
  }
  
  
  
}


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
                                       buffer = 0.003, 
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
  
  wh      <- c(wh_1[-c(1:n_rem, length(wh_1):(length(wh_1)+1-n_rem))],
               wh_2[-c(1:n_rem, length(wh_2):(length(wh_2)+1-n_rem))])
  
  wh
}


#' find_reference
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
find_reference <- function(x, ...) {
  
  # calculate correlation with probes
  calib_t <- x[["trace_time"]][["ref_temperature"]]

  wh <- correlate_with_temperature(x, calib_t, ...)
  
  x$trace_distance[wh, reference := TRUE]
  
  x
  
}


#' find_water_bath
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
find_water_bath <- function(x, ...) {
  
  # calculate correlation with probes
  calib_t <- x[["trace_time"]][["calib_temperature"]]
  
  wh <- correlate_with_temperature(x, calib_t, ...)
  
  x$trace_distance[wh, bath := TRUE]
  
  x
}  
 

#' #' find_water_bath_2
#' #'
#' #' @param x 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' find_water_bath_2 <- function(x) {
#'   
#'   calib_t <- x$trace_time$calib_temperature
#'   
#'   
#'   
#'   # unheated traces
#'   # d <- x$trace_distance[heated == FALSE][, list(distance)]
#'   # 
#'   # z <- x$trace_data[d, on = 'distance']
#'   z <- t(to_matrix(x$trace_data))
#'   z <- abs(z - calib_t)
#'   
#'   # look the median differences and the standard deviation of differences 
#'   med <- apply(z, 2, median)
#'   mad <- apply(z, 2, mad)
#'   
#'   q_med <- quantile(med, 0.01) 
#'   q_mad <- quantile(mad, 0.01)
#'   
#'   wh <- which(med < q_med * 3 & mad < q_mad * 2)
#'   
#'   x$trace_distance[wh, bath := TRUE]
#'   
#'   # mn_mad  <- min(mad)
#'   # mn_med  <- min(med)
#'   
#'   # sd_min <- quantile(frollapply(med, n = 13, FUN = mad, align = 'center'), 
#'   #                    na.rm = TRUE,
#'   #                    probs = 0.01)
#'   # sel <- which(mad < (mn_mad + sd_min * 7))
#'   # 
#'   # x$trace_distance[d[sel], bath := TRUE, on ='distance']
#'   
#'   x
#'   
#' }

# dts <- half_data(dts)
# 
# bp <- time_breakpoints(dts, shift = -20)
# 
# 
# sub <- subset_time(dts, as.POSIXct(0, tz = 'UTC', origin = '1970-01-01'), bp[1])
# sub <- subset_distance(sub, 50, 70)
# 
# # x <- to_matrix(sub)
# # fig <- plot_ly(z = x,
# # 
# #                type = "heatmap") %>%
# #   hide_colorbar()
# # fig
# 
# plot(cold_spray~distance, test)
