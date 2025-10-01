#' select_time
#'
#' @param x data read from a dts
#' @param times
#' @param ...
#'
#' @return
#' @rdname select_time
#' @export
#'
#' @examples
select_time <- function(x, times, ...) UseMethod("select_time")


#' @rdname select_time
#' @export
select_time.dts_long <- function(x, times) {
  x$trace_data <- select_time(x$trace_data, times)
  x$trace_time <- select_time(x$trace_time, times)

  x
}


#' @rdname select_time
#' @export
select_time.data.table <- function(x, times) {
  x[start %in% times]
}

#' select_time_ind
#'
#' @param x data read from a dts
#' @param inds
#' @param ...
#'
#' @return
#' @rdname select_time
#' @export
#'
#' @examples
select_time_ind <- function(x, inds, ...) UseMethod("select_time")


#' @rdname select_time_ind
#' @export
select_time_ind.dts_long <- function(x, inds) {
  x$trace_time <- x$trace_time[inds]
  x$trace_data <- select_time(x$trace_data, x$trace_time$start)

  x
}


#' select_distance
#'
#' @param x data read from a dts
#' @param distances
#' @param by
#' @param ...
#'
#' @return
#' @rdname select_distance
#' @export
#'
#' @examples
select_distance <- function(x, distances = NULL, by = NULL, ...) {
  UseMethod("select_distance")
}


#' @rdname select_distance
#' @export
select_distance.dts_long <- function(x, distances = NULL, by = NULL) {
  # find the distances where by is TRUE
  if (!is.null(by)) {
    distances <- get_distance_table(x)[get(by) == TRUE]$distance
  }

  x$trace_data <- select_distance(x$trace_data, distances)
  x$trace_distance <- select_distance(x$trace_distance, distances)

  x
}


#' @rdname select_distance
#' @export
select_distance.data.table <- function(x, distances = NULL) {
  x[distance %in% distances]
}
