#' get_temperature_breakpoint
#'
#' @param x
#' @param datetime_val
#' @param n_values
#' @param n_buffer
#' @param temp_col
#'
#' @return
#' @export
#'
#' @examples
get_temperature_breakpoint <- function(
  x,
  datetime_val,
  n_values = 1000,
  n_buffer = 2,
  temp_col = 'temperature'
) {
  UseMethod("get_temperature_breakpoint")
}


#' @rdname get_temperature_breakpoint
#' @export
get_temperature_breakpoint.dts_long <- function(
  x,
  datetime_val,
  n_values = 1000,
  n_buffer = 2,
  temp_col = 'temperature'
) {
  start_times <- get_times(x)
  tms <- which(start_times - datetime_val <= 0)
  tms <- head(tail(tms, n_values + n_buffer), n_values)
  times <- start_times[tms]

  dat <- x$trace_data[start %in% times]
  dat[, list(temperature_0 = mean(get(temp_col))), by = distance]
}
