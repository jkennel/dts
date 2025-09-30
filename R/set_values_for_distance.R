#' set_values_for_distance
#'
#' @param x
#' @param variable_name
#' @param start_distance
#' @param end_distance
#' @param specific_depths
#' @param set_to
#'
#' @return
#' @export
#'
set_values_for_distance <- function(
  x,
  variable_name,
  begin,
  end,
  specific_depths = NA_real_,
  set_to = TRUE
) {
  if (!variable_name %in% names(x$trace_distance)) {
    cls <- class(set_to)
    na_class <- NA
    class(na_class) <- cls
    x$trace_distance[, (variable_name) := na_class]
  }

  if (!is.na(specific_depths)) {
    x$trace_distance[distance %in% specific_depths, (variable_name) := set_to]
  } else {
    for (i in seq_along(begin)) {
      x$trace_distance[
        between(distance, begin[i], end[i]),
        (variable_name) := set_to[i]
      ]
    }
  }

  invisible(x)
}
