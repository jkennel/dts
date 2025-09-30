#' write_dts
#'
#' @param x
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples
write_dts <- function(x, output_dir, ...) UseMethod("write_dts")


#' @rdname write_dts
#' @export
write_dts.dts_long <- function(x, output_dir = getwd(), ...) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  fwrite(get_distance_table(x), file.path(output_dir, 'dts_distances.csv'))
  fwrite(get_time_table(x), file.path(output_dir, 'dts_time.csv'))
  fwrite(get_data_table(x), file.path(output_dir, 'dts_data.csv'))
}


#' write_dts_to_wellcad
#'
#' @param x
#' @param output_dir
#' @param max_columns maximum columns that wellcad can handle
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write_dts_to_wellcad <- function(x, output_dir, ...) {
  UseMethod("write_dts_to_wellcad")
}


#' @rdname write_dts_to_wellcad
#' @export
write_dts_to_wellcad.dts_long <- function(
  x,
  output_dir = getwd(),
  max_columns = 1000,
  id = "",
  ...
) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  m <- to_matrix(get_data_table(x))

  colnames(m) <- as.character(get_time_table(x)$start)

  n <- ncol(m)
  subs <- seq(1, n, max_columns)

  for (i in subs) {
    end_col <- pmin(i + max_columns, n)
    fn <- file.path(
      output_dir,
      paste0(id, 'dts_data_', i, '_', end_col, '.csv')
    )
    out <- cbind(
      distance = get_distance_table(x)$distance,
      as.data.table(m[, i:end_col])
    )

    fwrite(out, fn, eol = "\r\n")
  }
}
