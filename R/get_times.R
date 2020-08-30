#' get_times
#'
#' @param x 
#' @param type 'start', 'end', or 'mid' 
#'
#' @return
#' @export
#'
#' @examples
#' 
get_times <- function(x, type = 'start', ...) UseMethod("get_times")


#' @rdname get_times
#' @export
get_times.dts <- function(x, type = 'start') {
  
  as.POSIXct(map_dbl(x$dts, .f = function(x, ...) {
    x$trace_time$start
  }), origin = '1970-01-01', tz = 'UTC')
  
}


#' @rdname get_times
#' @export
get_times.dts_long <- function(x, type = 'start') {
  
  x$trace_time[[type]]
  
}