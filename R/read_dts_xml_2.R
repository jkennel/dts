
#' read_dts_xml_2
#'
#' @param in_dir path to the input folder
#' @param n_cores number of cores to use for parallel processing
#' @param date_format 'matlab' or 'R' format
#' @param max_files maximum number of files to read
#' @param return_stokes return stokes and antistokes
#' @param in_memory write to disk or return list of data
#' @param format output file type, csv, parquet, ...
#'
#' @return dts list of results
#' @export
#'
read_dts_xml_2 <- function(in_dir, 
                           out_dir = getwd(),
                           n_cores = 1, 
                           max_files = Inf,
                           return_stokes = FALSE, 
                           in_memory = FALSE,
                           format = 'csv', 
                           trim) {
  
  
  
  # get xml file names
  fn <- list.files(in_dir, 
                   full.names = TRUE, 
                   pattern = '*.xml$')
  fn <- sort(fn)
  
  
  # only read some of the files?
  if (length(fn) > max_files) {
    fn <- fn[1:max_files]
  }
  
  
  # parse one file to get the locations of the important text
  a <- readLines(fn[1], n = 500)
  
  
  # find the indices of certain components
  indices <- list(start_index = find_in_xml('<minDateTimeIndex>', a),
                  end_index = find_in_xml('<maxDateTimeIndex>', a),
                  acquisition_time_index = find_in_xml('<acquisitionTime>', a),
                  reference_temperature_index = find_in_xml('<referenceTemperature>', a),
                  probe_1_index = find_in_xml('<probe1Temperature>', a),
                  probe_2_index = find_in_xml('<probe2Temperature>', a),
                  forward_signal_averages_index = find_in_xml('<forwardSignalAverages>', a),
                  reverse_signal_averages_index = find_in_xml('<reverseSignalAverages>', a),
                  probe_1_voltage_index = find_in_xml('<probe1Voltage>', a),
                  probe_2_voltage_index = find_in_xml('<probe2Voltage>', a),
                  reference_probe_voltage_index = find_in_xml('<referenceProbeVoltage>', a)
  )
  
  # return all data or just temperature

    nms <- c('distance',
             'stokes',
             'anti_stokes', 
             'rev_stokes',
             'rev_anti_stokes', 
             'temperature')
  if (return_stokes) {
    col_indices <- 1L:6L
  } else {
    col_indices <- c(1L, 6L)
  }
  
  folder_path <- file.path(out_dir, format, 'dts_data')
  
  
  if(!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
  
  # overwrite existing files
  if(!in_memory & format == 'csv') {
    unlink(file.path(folder_path, 'dts_data.csv'))
    unlink(file.path(folder_path, 'dts_time_info.csv'))
    data.table::fwrite(as.list(c(nms[col_indices], 'start')), 
                       file.path(folder_path, 'dts_data.csv'))
    for(i in seq_along(1:n_cores)){
      
      data.table::fwrite(as.list(c(nms[col_indices])), file.path(folder_path, paste0(i,'_dts_data.csv')))
    }
  }
  
  
  # set up parallel cluster
  
  fn_l <- split(fn, 1:n_cores)
  
  cl <- parallel::makePSOCKcluster(n_cores)
  parallel::clusterExport(cl=cl, 
                          varlist = c('indices', 
                                      'nms', 
                                      'folder_path', 
                                      'col_indices',
                                      'fn_l'), 
                          envir=environment())
  
  dts <- parallel::parLapply(cl, 1:n_cores, function(i) {
    
    fn_sub <- fn_l[[i]]
    
    for (x in fn_sub) {
    b <- readLines(x, n = 30)
    
    # read in meta data for trace
    vals <- vector(mode = "list", length = length(indices))
    for(ind in seq_along(indices)) {
      vals[[ind]] <- dts::get_value_xml(b[indices[[ind]]])
    }
    names(vals) <- names(indices)
    
    
    # fast read 
    a <- dts::read_file_cpp(x)
    
    # find the start and end of the data
    s <- regexpr('<logData>', a, fixed =TRUE)[[1]][1] + 9
    e <- regexpr('</logData>', a, fixed =TRUE)[[1]][1]
    
    cat(stringi::stri_replace_all_fixed(
      substr(a, s, e),
      pattern = c('<data uid="measurement">',
                  '</data>'),
      replacement = c(""),
      vectorize_all = FALSE), file = file.path(folder_path, paste0(i, '_dts_data.csv')), append = TRUE)
    # pull out data string
    # z <- data.table::fread(
    #   stringi::stri_replace_all_fixed(
    #     substr(a, s, e),
    #     pattern = c('<data uid="measurement">',
    #                 '</data>'),
    #     replacement = c(""),
    #     vectorize_all = FALSE),
    #   select = col_indices,
    #   col.names = nms[col_indices],
    #   sep = ',',
    #   strip.white = FALSE,
    #   blank.lines.skip = TRUE)
    # 
    # # add start time
    # data.table::set(z, j = 'start', value = as.numeric(fasttime::fastPOSIXct(vals[1])))
    
    if(in_memory) {
      return(z)
    }
    
    # write files to disk - make into a function
    if(format == 'csv') {
      
      # data.table::fwrite(z,
      #                    file = file.path(folder_path, paste0(i, '_dts_data.csv')),
      #                    append = TRUE,
      #                    quote = FALSE,
      #                    col.names = FALSE)
      
    } else if (format == 'parquet'){
      arrow::write_parquet(z,
                           file.path(folder_path, 
                                     paste0(gsub('.xml', '', basename(x)), '.parquet')))
      
    } else if (format == 'feather'){
      arrow::write_feather(z, 
                           file.path(folder_path, 
                                        paste0(gsub('.xml', '', basename(x)), '.feather')))
      
    } else if (format == 'fst'){
      fst::write_fst(z, 
                     file.path(folder_path, 
                                  paste0(gsub('.xml', '', basename(x)), ".fst")))
      
    } else {
      stop('format not supported.')
    }
    
    }
    
    return(as.list(vals))
  })
  
  # stop cluster
  parallel::stopCluster(cl)

  if(in_memory) {
    return(data.table::rbindlist(dts))
  } 
  
  # ds <- arrow::open_dataset(folder_path, format = format)
  # 
  # return(list(trace_data = ds, trace_time = rbindlist(dts)))


}


# system.time(a <- read_dts_xml_2('/home/jonathankennel/Storage/tmp/channel 1',
#                                 max_files = 5000,
#                                 n_cores = 12,
#                                 return_stokes = FALSE,
#                                 in_memory = FALSE,
#                                 format = 'parquet'))
# system.time(a <- read_dts_xml_2('/home/jonathankennel/Storage/tmp/channel 1',
#                                 max_files = 5000,
#                                 n_cores = 12,
#                                 return_stokes = FALSE,
#                                 in_memory = FALSE,
#                                 format = 'csv'))
# system.time(a <- read_dts_xml_2('/home/jonathankennel/Storage/tmp/channel 1',
#                                 # max_files = 5000,
#                                 n_cores = 12,
#                                 return_stokes = FALSE,
#                                 in_memory = TRUE,
#                                 format = 'csv'))

# system.time(a <- read_dts_xml_2('/home/jonathankennel/Storage/tmp/channel 1',
#                                 max_files = 5000,
#                                 n_cores = 12,
#                                 in_memory = FALSE,
#                                 format = 'feather'))

# system.time(a <- read_dts_xml_2('/home/jonathankennel/Storage/tmp/channel 1',
#                                 max_files = 5000,
#                                 n_cores = 12,
#                                 return_stokes = FALSE,
#                                 in_memory = FALSE,
#                                 format = 'fst'))

# tmp <- list.files('fst/dts_data/', full.names = TRUE)
# system.time({a <- lapply(tmp, fst)})
# system.time({b <- fread('csv/dts_data/dts_data.csv')})
# fst(c(tmp[2]), old_format = FALSE)

# system.time(ds <- arrow::open_dataset(file.path('parquet/dts_data'), format = 'parquet'))
# system.time(ds <- arrow::read_csv_arrow('csv/dts_data/dts_data.csv',
#                                         as_data_frame = FALSE))
# # system.time(ds2 <- data.table::fread('csv/dts_data/dts_data.csv',
# #                                      colClasses = c('numeric', 'numeric', 'POSIXct')))
# system.time(ds2 <- data.table::fread('csv/dts_data/dts_data.csv',
#                                      colClasses = c('numeric', 'numeric', 'numeric')))
# system.time(ds2 <- duckdb::duckdb_read_csv(con, files = 'csv/dts_data/dts_data.csv', 
#                                      name = 'test', nrow.check = 2))
# system.time(ds <- arrow::open_dataset(file.path('feather/dts_data'), format = 'feather'))
# 
# system.time({
#   


# # z <- fread(file.path('csv/dts_data/dts_data.csv'))
# library("DBI")
# library('dplyr')
# library(duckdb)
# con = dbConnect(duckdb::duckdb(), dbdir=":memory:", read_only=FALSE)
# arrow::to_duckdb(ds, con, table_name = 'test')
# # system.time(a <- setDT(dbGetQuery(con, "SELECT distance, date_part('h', start) AS hour, date_part('m', start) AS min, avg(temperature) FROM test GROUP BY distance, hour, min")))
# system.time(a <- dbGetQuery(con, "SELECT distance, start / 60 AS start, avg(temperature) AS temperature FROM test GROUP BY distance, start / 60"))
# system.time({d <- ds2[, mean(temperature), by = list(distance, start %/% 60)]})
# # system.time({d <- ds2[, mean(rev_anti_stokes), by = list(V1, time = as.numeric(temperature) %/% 60)]})
# duckdb::dbDisconnect(con)
#
# # a <- ds |>
# #   to_duckdb()




# b <- ds |>
#   select(distance) |>
#   filter(distance < -60) |>
#   mutate(new = 1) |>
#   to_duckdb() |>
#   slice_max(distance, n = 1, with_ties = FALSE)
# 
# tmp <- a |> 
#   inner_join(b, by = 'distance') |>
#   collect()
# 
# })

# system.time({
# con <- dbConnect(duckdb(), dbdir = 'a.db')
# sql_string <- paste0("CREATE TABLE people AS SELECT * FROM parquet_scan('",'/home/jonathankennel/Storage/r_packages/dts/parquet/dts_data/*.parquet',"')")
# dbExecute(con, sql_string)
# dbDisconnect(con, shutdown = TRUE)
# })

# 
# microbenchmark::microbenchmark(
#   gregexpr('<logData>', a, fixed =TRUE)[[1]][1],
#   gregexpr('</logData>', a, fixed =TRUE)[[1]][1],
#   gregexpr('(?<=logData>).*?(?=<\\/data>)', a, perl=TRUE),
#   tmp <- substr(a, 20859, 255370),
#   times = 2
# )
# 
# 
# 
# tmp <- bench::mark(
#   a <- data.table::fread('a.csv'),
#   b <- arrow::read_csv_arrow('a.csv', as_data_frame = FALSE),
#   check = FALSE
# ) |> data.table::as.data.table()
# 
# microbenchmark::microbenchmark(
# a[V1 > 100],
# b[b$V1 > 100,], times = 2
# )
# 
# bench::mark(
#   as.numeric(b$datetime),
#   as.numeric(a$datetime)
# ) |> data.table::as.data.table()
# # library(readr)
# # microbenchmark::microbenchmark(
# #   a <- vroom::vroom_lines(fn[1], num_threads = 10),
# #   b <- readLines(fn[1])
# # )
# 
#     dt <- grep('data>$', a)
#     
# system.time({
#   con  = file('a.csv', open = "a")
#   for (i in 1:1000) {
#     a <- readLines(fn[i])[dt]
#     b <- gsub("[^0-9.,-]", "", a, perl = TRUE)
#     # b <- regmatches(a, gregexpr("-?\\d*\\.{0,1}\\d+", a, perl = TRUE))
#     # b <- regmatches(a, gregexpr('(?<=measurement\">).*?(?=<\\/data>)', a, perl=TRUE))
#     # b <- gsub("([[:alpha:]])", "", a, perl = TRUE)
#     # data.table::set(z, j = 'datetime', value = dt)
#     # data.table::fwrite(z, 'a.csv',
#     #                    append = TRUE,
#     #                    nThread = 1)  }
#   }
#   close(con)
# })
# 
# tmp <- paste0(sapply(fn, read_file_cpp2), collapse = '\n')
# system.time({
# regmatches(tmp, gregexpr('(?<=t\">).*?(?=<\\/data>)', tmp, perl=TRUE, useBytes = TRUE))[[1]]
# })

# 
# system.time({
#   # for (i in 1:1000) {
#   system(paste0("sed -e 's/>\\(.*\\)\\/data>/\\1/' ", '*.xml', " > ", "out.csv"))
#   # }
# })
# 
# 
# system.time({
# 
#   system(paste0("rg -j8 -m2000 '</data>' ", '*.xml', " > ", "out.csv"))
# 
# })

