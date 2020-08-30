#' subset_time
#'
#' @param x data read from a dts
#' @param begin
#' @param end
#' @param ... 
#'
#' @return
#' @rdname subset_time
#' @export
#'
#' @examples
subset_time <- function(x, begin, end, ...) UseMethod("subset_time")


#' @rdname subset_time
#' @export
subset_time.dts_long <- function(x, begin, end) {
  
  x$trace_data <- subset_time(x$trace_data, begin, end)
  x$trace_time <- subset_time(x$trace_time, begin, end)
  
  x
  
}


#' @rdname subset_time
#' @export
subset_time.data.table <- function(x, begin, end) {
  
  x[between(start, begin, end)]
  
}



#' subset_distance
#'
#' @param x data read from a dts
#' @param begin
#' @param end
#' @param ... 
#'
#' @return
#' @rdname subset_distance
#' @export
#'
#' @examples
subset_distance <- function(x, begin, end, ...) UseMethod("subset_distance")


#' @rdname subset_distance
#' @export
subset_distance.dts_long <- function(x, begin, end) {
  
  x$trace_data <- subset_distance(x$trace_data, begin, end)
  x$trace_distance <- subset_distance(x$trace_distance, begin, end)
  
  x
  
}


#' @rdname subset_distance
#' @export
subset_distance.data.table <- function(x, begin, end) {
  
  x[between(distance, begin, end)]
  
}