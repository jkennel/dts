find_coldspray <- function(x) {
  
  test <- x$trace_data[, list(cold_spray = temperature - frollmean(temperature, 180)),
                       by = 'distance']
  
  # test <- test[, list(min(cold_spray, na.rm=TRUE),
  #                     max(cold_spray, na.rm=TRUE)), by = distance]
  # 
  test  
}



#' find_water_bath
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
find_water_bath <- function(x) {
  
  calib_t <- x$trace_time$calib_temperature
  
  # unheated traces
  d <- x$trace_distance[heated == FALSE][, list(distance)]
  
  z <- x$trace_data[d, on = 'distance']
  z <- abs(t(to_matrix(z)) - calib_t)
   
  # look the median differences and the standard deviation of differences 
  med <- apply(z, 2, median)
  mad <- apply(z, 2, mad)

  mn_mad  <- min(mad)
  mn_med  <- min(mad)

  sd_min <- min(frollapply(med, n = 13, FUN = mad, align = 'center'), na.rm = TRUE)
  sel <- which(mad < (mn_mad + sd_min * 8))

  x$trace_distance[d[sel], bath := TRUE, on ='distance']
  
  x
  
}

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
