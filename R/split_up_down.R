#' split_up_down
#' sets the zero point for a well and recalculates distance based on the start,
#' termination and end of the well's cable
#'
#' @param x
#' @param begin
#' @param termination
#' @param end
#' @return
#'
#' @rdname split_up_down
#' @export
#'
#' @examples
split_up_down <- function(x, begin, termination, end, ...) {
  UseMethod("split_up_down")
}


#' @rdname split_up_down
#' @export
split_up_down.data.table <- function(x, begin, termination, end, ...) {
  x[, up_down := NA_character_]

  x[between(start, begin, termination), up_down := "down"]
  x[between(start, termination, end), up_down := "up"]

  x[up_down == "down", distance := distance - begin]
  x[up_down == "up", distance := end - distance]

  x
}


#' @rdname split_up_down
#' @export
split_up_down.dts_long <- function(x, begin, termination, end, ...) {
  x <- subset_distance(x, begin, end)

  x$trace_data <- split_up_down(get_data_table(x), begin, termination, end)
  x$trace_distance <- split_up_down(
    get_distance_table(x),
    begin,
    termination,
    end
  )
  x
}
