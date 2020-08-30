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


#' @rdname generate_report
#' @export
write_dts.dts_long <- function(x, output_dir = getwd(), ...) {

  fwrite(get_distance_table(x), file.path(output_dir, 'dts_distances.gz'))
  fwrite(get_time_table(x),  file.path(output_dir, 'dts_time.gz'))
  fwrite(get_data_table(x),  file.path(output_dir, 'dts_data.gz'))
  
}
