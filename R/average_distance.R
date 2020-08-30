group_distance <- function(x, interval) {
  
  (x %/% interval) * interval + interval / 2
  
}


#' average_distance
#'
#' @param x data read from a dts
#' @param n
#'
#' @return
#' @rdname average_distance
#' @export
#'
#' @examples
average_distance <- function(x, n) UseMethod("average_distance")




#' @rdname average_distance
#' @export
average_distance.dts_long <- function(x, n = 0.5) {
  
  x$trace_data <- x$trace_data[,
                               lapply(.SD, mean),
                               by = list(start, distance = group_distance(distance, n)),
                               .SDcols = is.numeric]
  
  x
}