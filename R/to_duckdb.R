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


#' to_duckdb_2
#'
#' @param file_path
#' @param db_dir
#' @param n_cores
#' @param max_files
#' @param return_stokes
#' @param output_rds
#' @param trim
#'
#' @return
#' @export
#'
#' @examples
to_duckdb_2 <- function(
  file_path,
  db_dir,
  n_cores = 1L,
  max_files = Inf,
  return_stokes = FALSE,
  trim = TRUE
) {
  t_dir <- file.path(tempdir(), 'dts', round(as.numeric(Sys.time())))
  unzip(file_path, exdir = t_dir, junkpaths = TRUE)

  # get, subset, and sort xml file names
  fn <- list.files(t_dir, full.names = FALSE, pattern = '*.xml$')

  fn <- sort(fn, method = 'radix')[1:pmin(length(fn), max_files)]

  fn <- file.path(t_dir, fn)

  # get the constant meta data from the first XML file
  meta <- read_one_xml(fn[1])

  type <- meta$device$type
  if (type == 'xt') {
    data_pattern <- c('<data>\n', '</data>\n')
  } else {
    data_pattern <- c('<data uid="measurement">', '</data>')
  }

  # set column names
  double_ended <- meta[['device']][['double_ended']]
  if (double_ended) {
    if (return_stokes) {
      select <- c(1L:6L)
    } else {
      select <- c(1L, 6L)
    }
    nms <- c(
      'distance',
      'stokes',
      'anti_stokes',
      'rev_stokes',
      'rev_anti_stokes',
      'temperature'
    )[select]
  } else {
    if (return_stokes) {
      select <- c(1L:4L)
    } else {
      select <- c(1L, 4L)
    }
    nms <- c('distance', 'stokes', 'anti_stokes', 'temperature')[select]
  }

  # strings to find in xml
  keys <- xml_key(type)

  # path for the output
  # folder_path <- file.path(out_dir, 'dts_data')

  # if (!dir.exists(folder_path)) {
  #   dir.create(folder_path, recursive = TRUE)
  # }

  # overwrite existing files (the csv file is treated as disposable)
  # out_file <- file.path(folder_path, 'dts_data.csv')
  # unlink(out_file)

  # write the new file with header
  # fwrite(as.list(c(nms, 'start')), out_file)

  skip <- 0L
  if (trim) {
    skip <- which(meta[['distance']] >= 0)[1L]
    meta[['distance']] <- meta[['distance']][-(1L:(skip - 1L))]
  }

  # set up parallel cluster
  cl <- parallel::makePSOCKcluster(n_cores)
  parallel::clusterExport(
    cl = cl,
    varlist = c(
      # 'folder_path',
      'select',
      'type',
      'keys',
      'data_pattern',
      'skip'
    ),
    envir = environment()
  )

  dts <- parallel::parLapply(cl, fn, function(x) {
    # Fast read
    dat_list <- list()
    vals_list <- list()

    xml_text <- dts::read_file_cpp(x)
    # Find the start and end of the data
    if (type == 'xt') {
      s <- regexpr('<data>', xml_text, fixed = TRUE)[[1]][1] + 6L
      e <- regexpr('</logData>', xml_text, fixed = TRUE)[[1]][1] - 1L
      bot <- substr(xml_text, e, e + 900L)

      vals <- as.list(c(
        as.numeric(fasttime::fastPOSIXct(stringi::stri_match_first_regex(
          xml_text,
          keys$pattern[1L:2L]
        )[, 2L])),
        as.numeric(stringi::stri_match_first_regex(bot, keys$pattern[3L:11L])[,
          2
        ])
      ))
    } else {
      s <- regexpr('<logData>', xml_text, fixed = TRUE)[[1]][1] + 9L
      e <- regexpr('</logData>', xml_text, fixed = TRUE)[[1]][1] - 1L

      vals <- stringi::stri_match_first_regex(xml_text, keys$pattern)[, 2L]
      vals <- as.list(c(
        as.numeric(fastPOSIXct(vals[1L:2L])),
        as.numeric(vals[3L:11L])
      ))
    }

    # Read in the data
    dat <- data.table::fread(
      stringi::stri_replace_all_fixed(
        substr(xml_text, s, e),
        pattern = data_pattern,
        replacement = c(""),
        vectorize_all = FALSE
      ),
      skip = skip,
      select = select,
      colClasses = 'numeric',
      blank.lines.skip = TRUE,
      nThread = 1L
    )

    # Add start time
    data.table::set(dat, j = 'start', value = vals[[1]])

    # data.table::setDT(vals)
    list(dat, vals)
  })

  # stop cluster
  parallel::stopCluster(cl)

  unlink(t_dir, recursive = TRUE)

  dat <- rbindlist(lapply(dts, "[[", 1L))
  setnames(dat, c(nms, 'start'))
  dat[, start := as.POSIXct(start, tz = 'UTC')]
  setkey(dat, start, distance)

  dts <- rbindlist(lapply(dts, "[[", 2L))
  setnames(dts, keys$names)

  set(
    dts,
    j = 'calib_temperature',
    value = (dts[['probe_1']] + dts[['probe_2']]) / 2.0
  )

  dts[, mid := (start + end) / 2.0]
  dts[, start := as.POSIXct(start, tz = 'UTC')]
  dts[, mid := as.POSIXct(mid, tz = 'UTC')]
  dts[, end := as.POSIXct(end, tz = 'UTC')]

  setkey(dts, start)

  # distance table
  distance <- data.table(
    distance = meta$distance,
    wh = meta$distance %between% c(0.0, meta[['device']][['fibre_length']]),
    junction = FALSE,
    heated = FALSE,
    bath = FALSE,
    reference = FALSE,
    borehole = FALSE
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_dir, read_only = FALSE)
  duckdb::dbWriteTable(con, "trace_data", dat, overwrite = TRUE)
  duckdb::dbWriteTable(con, "trace_time", dts, overwrite = TRUE)
  duckdb::dbWriteTable(con, "trace_distance", distance, overwrite = TRUE)
  duckdb::dbWriteTable(con, "device", meta$device, overwrite = TRUE)
  duckdb::dbWriteTable(con, "channels", meta$channels, overwrite = TRUE)
  DBI::dbDisconnect(con, shutdown = TRUE)

  return(0)
}

# library(dts)
# system.time(
#   a <- to_duck_db_2(
#     "../../scratch/Gun Club 2024-11-19 ADTS.zip",
#     db_dir = "tmp.duckdb",
#     n_cores = 16L,
#     return_stokes = TRUE
#   )
# )
# plot_distances(a)
