#' bath_calibration
#'
#' @param x data read from a dts
#'
#' @return
#' @rdname bath_calibration
#' @export
#'
#' @examples
bath_calibration <- function(x, ...) UseMethod("bath_calibration")



#' @rdname bath_calibration
#' @export
bath_calibration.dts_long <- function(x, ...) {
  
  bath_distance <- x$trace_distance[bath == TRUE]$distance

  bath_dts <- x$trace_data[distance %in% bath_distance, 
               list(bath_temp_dts = mean(temperature)), by = start]
  

  # add bath temperature
  x$trace_time[bath_dts, bath_temp_dts := bath_temp_dts, on = 'start']
  
  # adjustment to apply
  x$trace_time[, calib_adj := bath_temp_dts - calib_temperature]#runmed(calib_temperature, 7)]

  x$trace_data[x$trace_time, temperature := temperature - calib_adj, on = 'start']
  
  invisible(x)

}





#' # need to clean this
#' #' get_bath
#' #'
#' #' @param x 
#' #' @param start_distance 
#' #' @param end_distance 
#' #' @param cutoff 
#' #' @param pad 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' get_bath <- function(x, 
#'                      start_distance = 0.0,
#'                      end_distance = 300, 
#'                      cutoff = 0.04,
#'                      pad = 10) {
#'   
#'   
#'   distance_junction <- get_junction_distance(x)
#'   
#'   std_dev <- x$trace_data[between(distance, start_distance, end_distance),
#'                           list(sd_temp = sd(temperature)), by = distance]
#'   std_dev[abs(distance - distance_junction) < 1, sd_temp := NA_real_]
#'   
#'   std_dev[, sd_temp := sd_temp - min(frollmean(sd_temp, pad), na.rm=TRUE)]
#'   std_dev[, bath := sd_temp < cutoff]
#'   
#'   runs <- rle(std_dev$bath)
#'   run_start <- runs$values[1]
#'   runs$values[runs$lengths < pad*2] <- FALSE
#'   runs <- rep(runs$values, times = runs$lengths)
#'   
#'   std_dev[, bath := runs]
#'   
#'   std_dev[,type := 'down']
#'   std_dev[distance >= distance_junction, type := 'up']
#' 
#'   # pad
#'   std_dev[, bath := c(rep(FALSE, pad),
#'                         bath[(pad+1):(.N-pad)],
#'                         rep(FALSE, pad)), by = list(bath_old = bath, type)]
#'   
#'   # plot(sd_temp~distance, std_dev, log = 'y', col = as.numeric(1 + bath))
#'   std_dev
#'   
#' }
#' 
#' 
#' 
#' 
#' #' get_junction_distance
#' #'
#' #' @param x 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' get_junction_distance <- function(x) {
#'   
#'   bp <- time_breakpoints(x, shift = -20)
#'   
#'   m <- to_matrix(get_heating(x, bp))[, 1:100]
#'   n <- nrow(m)
#'   cor_heat <- cor_by_trace(m, 1)
#'   cor_heat[c(1:10, n:(n - 10))] <- NA_real_
#'   
#'   m <- to_matrix(get_cooling(x, bp))[, 1:100]
#'   n <- nrow(m)
#'   cor_cool <- cor_by_trace(m, 1)
#'   cor_cool[c(1:10, n:(n - 10))] <- NA_real_
#'   
#'   cor_dts <- (cor_cool + cor_heat)
#'   
#'   cor_dts[cor_dts < 1.85] <- NA_real_
#'   wh <- which(!is.na(cor_dts))
#'   wh[1]
#'   plot(cor_dts, type='l')
#'   
#'   as.numeric(rownames(m)[which.min(cor_distance)+1])
#' 
#' }
