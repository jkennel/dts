#' split_distance_type
#'
#' @param x data read from a dts
#' @param type_col
#' @param ...
#'
#' @return
#' @rdname split_distance_type
#' @export
#'
#' @examples
split_distance_type <- function(x, type_col = NULL, ...) {
  UseMethod("split_distance_type")
}


#' @rdname split_distance_type
#' @export
split_distance_type.dts_long <- function(x, type_col = NULL) {
  distances <- NULL
  d <- get_distance_table(dts)
  types <- unique(d[[type_col]])

  result <- list()

  for (i in seq_along(types)) {
    tmp <- copy(x)
    distances <- d[which(d[[type_col]] == types[i])][["distance"]]
    tmp$trace_data <- select_distance(tmp$trace_data, distances)
    tmp$trace_distance <- select_distance(x$trace_distance, distances)
    result[[i]] <- copy(tmp)
  }

  names(result) <- types
  result
}
