#' resample_distance
#'
#' @param x a dts object that you want to spatially resample 
#' @param y the dts object or vector of distances for the resampling
#' @param ... arguments to pass to approx
#'
#' @return a dts object that has been resampled
#' @export
#'
#' @examples
resample_distance <- function(x, y, ...) {
  
  # get the interpolation distances
  if(inherits(y, 'dts_long')) {
    y <- get_distance_table(y)[['distance']]
  } 
  
  # which columns to interpolate
  nms <- setdiff(names(get_data_table(x)), c('start', 'distance'))
  
  # interpolate data
  x$trace_data <- get_data_table(x)[, 
       c(list(distance = y),
         lapply(.SD, function(z) {
         approx(x = distance,
                y = z,
                xout = y, 
                ...)$y})), 
         by = start, .SDcols = nms]
  
  
  # update distance table
  roll_to <- data.table(distance = y)
  x$trace_distance <- get_distance_table(x)[ roll_to, on = 'distance', roll = TRUE]
    
  invisible(x)
  
}

