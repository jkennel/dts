#' set_values_for_distance
#'
#' @param x
#' @param variable_name
#' @param start_time
#' @param end_time
#' @param specific_times
#' @param set_to
#'
#' @return
#' @export
#'
set_values_for_time <- function(
  x,
  variable_name,
  begin,
  end,
  specific_times = NA_real_,
  set_to = TRUE
) {
  if (!variable_name %in% names(x$trace_time)) {
    cls <- class(set_to)
    na_class <- NA
    class(na_class) <- cls
    x$trace_time[, (variable_name) := na_class]
  }

  if (!is.na(specific_times)) {
    x$trace_time[distance %in% specific_times, (variable_name) := set_to]
  } else {
    x$trace_time[between(start, begin, end), (variable_name) := set_to]
  }

  invisible(x)
}
