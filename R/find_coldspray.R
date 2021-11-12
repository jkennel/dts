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



#' find_reference
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
find_reference <- function(x, ...) {
  
  reference <- NULL
  
  # calculate correlation with probes
  calib_t <- x[["trace_time"]][["ref_temperature"]]

  wh <- correlate_with_temperature(x, calib_t, ...)
  
  x$trace_distance[wh, reference := TRUE]
  
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
# #                type = "heatmap") |>
# #   hide_colorbar()
# # fig
# 
# plot(cold_spray~distance, test)
