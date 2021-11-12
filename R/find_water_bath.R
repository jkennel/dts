#' find_water_bath
#'
#' @param x the dts_long dataset
#' @param ... arguments to pass to correlate_with_temperature
#'
#' @return
#' @export
#'
#' @examples
find_water_bath <- function(x, ...) {
  
  bath <- NULL
  
  # calculate correlation with probe temperature
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
