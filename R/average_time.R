#' group_time
#'
#' @param x time vector that can be coerced to numeric
#' @param interval size of interval in time units
#'
#' @return a vector where each time value is assigned the interval midpoint
#' @export
#'
#' @examples
group_time <- function(x, interval) {
  as.POSIXct(
    (as.numeric(x) %/% interval) * interval + interval / 2,
    origin = '1970-01-01',
    tz = 'UTC'
  )
}


#' average_time
#'
#' @param x data read from a dts
#' @param n size of the interval to average in seconds
#'
#' @return
#' @rdname average_time
#' @export
#'
#' @examples
average_time <- function(x, n) UseMethod("average_time")


#' @rdname average_time
#' @export
average_time.dts_long <- function(x, n = 30) {
  wh <- sapply(get_data_table(x), is.numeric)

  wh['distance'] <- FALSE
  x$trace_data <- get_data_table(x)[,
    lapply(.SD, mean),
    by = list(start = group_time(start, n), distance),
    .SDcols = wh
  ]

  x$trace_time <- get_time_table(x)[,
    c(
      n = .N,
      max_time = max(end),
      mean_time = mean(mid),
      min_time = min(start),
      lapply(.SD, mean)
    ),
    by = list(time_interval_center = group_time(start, n), type = type),
    .SDcols = sapply(get_time_table(x), is.numeric)
  ]
  setnames(x$trace_time, 'time_interval_center', 'start')

  class(x) <- c('dts_time_subset', class(x))
  x
}


#' log_group_time
#'
#' @param x time vector that can be coerced to numeric
#' @param interval size of interval in time units
#'
#' @return a vector where each time value is assigned the interval midpoint
#' @export
#'
#' @examples
log_group_time <- function(x, interval) {
  x = round(x / interval, 0) * interval
}


#' log_average_time
#'
#' @param x data read from a dts
#' @param n size of the interval to average in seconds
#'
#' @return
#' @rdname log_average_time
#' @export
#'
#' @examples
log_average_time <- function(x, n) UseMethod("log_average_time")


#' @rdname log_average_time
#' @export
log_average_time.dts_long <- function(
  x,
  time_column = "log_elapsed_time",
  n = 100
) {
  interval <- 1.0 / n

  wh <- sapply(get_data_table(x), is.numeric)

  wh['distance'] <- FALSE

  x$trace_data <- get_data_table(x)[,
    lapply(.SD, mean),
    by = list(
      time_interval_center = log_group_time(get(time_column), interval),
      distance
    ),
    .SDcols = wh
  ]

  x$trace_time <- get_time_table(x)[,
    c(
      n = .N,
      max_time = max(end),
      mean_time = mean(mid),
      min_time = min(start),
      lapply(.SD, mean)
    ),
    by = list(
      time_interval_center = log_group_time(get(time_column), interval),
      type = type
    ),
    .SDcols = sapply(get_time_table(x), is.numeric)
  ]

  x$trace_data[!is.finite(time_interval_center), time_interval_center := 0]
  x$trace_time[!is.finite(time_interval_center), time_interval_center := 0]

  class(x) <- c('dts_time_subset', class(x))
  x
}

#' #' @rdname average_time
#' #' @export
#' average_time.data_table <- function(x, n = 30) {
#'
#'   x<- x[,
#'         lapply(.SD, mean),
#'         by = list(start = group_time(start, n), distance),
#'         .SDcols = is.numeric]
#'
#'   x
#' }
