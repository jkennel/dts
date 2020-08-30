#' time_breakpoints
#'
#' @param x data read from a dts
#' @param shift distance between differences in number of samples
#' @param col_name 
#'
#' @return
#' @rdname time_breakpoints
#' @export
#'
#' @examples
time_breakpoints <- function(x, shift, col_name) UseMethod("time_breakpoints")


#' @rdname time_breakpoints
#' @export
time_breakpoints.default <- function(x, shift = -10, col_name = 'temperature') {
  
  time_breakpoints(to_matrix(x, col_name = col_name), shift)
  
}


#' @rdname time_breakpoints
#' @export
time_breakpoints.matrix <- function(x, shift = -10, col_name = 'temperature') {
  
  by_time  <- colMeans(diff_by_trace(x, shift, 1))
  rn <- as.POSIXct(as.numeric(colnames(x)), origin = '1970-01-01', tz = 'UTC')
  
  list(on  = rn[which.min(by_time)], 
       off = rn[which.max(by_time)])
  
}




