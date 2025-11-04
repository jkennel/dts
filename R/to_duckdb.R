#' to_duckdb
#'
#' @param x data read from a dts
#' @param dbdir name of output file location
#'
#' @param ... arguments to pass to read_dts_xml_3 time_aggregate_interval,
#'     n_cores, trim, return_stokes, max_files, in_memory
#'
#' @return saves a duckdb database
#' @rdname to_duckdb
#' @export
#'
#' @examples
to_duckdb <- function(x, dbdir, ...) {
  ext <- tools::file_ext(x)

  if (ext == "zip") {
    dts <- read_dts_zip(x, ...)
    print(str(dts))
  } else {
    dts <- read_dts_xml_3(x, ...)
  }

  # connect to db
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir, read_only = FALSE)

  duckdb::dbWriteTable(con, "trace_data", dts$trace_data, overwrite = TRUE)
  duckdb::dbWriteTable(con, "trace_time", dts$trace_time, overwrite = TRUE)
  duckdb::dbWriteTable(
    con,
    "trace_distance",
    dts$trace_distance,
    overwrite = TRUE
  )
  duckdb::dbWriteTable(con, "device", dts$device, overwrite = TRUE)
  duckdb::dbWriteTable(con, "channels", dts$channels, overwrite = TRUE)
  DBI::dbDisconnect(con, shutdown = TRUE)

  dts
}


load_duckdb <- function(db_dir) {
  # connect to db
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_dir, read_only = TRUE)

  dts <- list()
  dts[["trace_data"]] <- setDT(DBI::dbReadTable(con, "trace_data"))
  dts[["trace_time"]] <- setDT(DBI::dbReadTable(con, "trace_time"))
  dts[["trace_distance"]] <- setDT(DBI::dbReadTable(con, "trace_distance"))
  dts[["device"]] <- setDT(DBI::dbReadTable(con, "device"))
  dts[["channels"]] <- setDT(DBI::dbReadTable(con, "channels"))
  class(dts) <- "dts_long"

  DBI::dbDisconnect(con, shutdown = TRUE)
  dts
}
