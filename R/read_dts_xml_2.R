
#' read_dts_xml_2
#'
#' @param in_dir path to the input folder
#' @param n_cores number of cores to use for parallel processing
#' @param date_format 'matlab' or 'R' format
#' @param max_files maximum number of files to read
#' @param return_stokes return stokes and antistokes
#' @param in_memory write to disk or return list of data
#'
#' @return dts list of results
#' @export
#'
read_dts_xml_2 <- function(in_dir, 
                           out_dir = getwd(),
                           n_cores = 1, 
                           max_files = Inf,
                           return_stokes = TRUE, 
                           in_memory = TRUE) {
  


  # get xml file names
  fn <- list.files(in_dir, 
                 full.names = TRUE, 
                 pattern = '*.xml$')
  fn <- sort(fn)
  
  
  # only read some of the files?
  if (length(fn) > max_files) {
    fn <- fn[1:max_files]
  }
  
  # overwrite existing files
  if(!in_memory) {
    unlink(file.path(out_dir, 'dts_data.csv'))
    unlink(file.path(out_dir, 'dts_time_info.csv'))
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
  
  
  # set up parallel cluster
  cl <- parallel::makePSOCKcluster(n_cores)
  parallel::clusterExport(cl=cl, 
                          varlist = c('indices'), 
                          envir=environment())
    
    nms <- c('distance', 
             'stokes',
             'anti_stokes', 
             'rev_stokes',
             'rev_anti_stokes', 
             'temperature')
  
    dts <- parallel::parLapply(cl, fn, function(x) {
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
    
    # pull out data string
    z <- data.table::fread(
      stringi::stri_replace_all_fixed(
        substr(a, s, e),
        pattern = c('<data uid="measurement">',
                    '</data>'),
        replacement = c(""),
        vectorize_all = FALSE),
      colClasses = rep('numeric', 6),
      sep = ',',
      col.names = nms, 
      strip.white = FALSE,
      blank.lines.skip = TRUE)
    
    # add start time
    data.table::set(z, j = 'start', value = fasttime::fastPOSIXct(vals[1]))
    
    # write files to disc
    if (!in_memory) {
      data.table::fwrite(z,
                         file = file.path(out_dir, 'dts_data.csv'),
                         append = TRUE,
                         quote = FALSE,
                         col.names = FALSE)
      data.table::fwrite(vals, 
                         file = file.path(out_dir, 'dts_time_info.csv'),
                         append = TRUE,
                         quote = FALSE,
                         col.names = FALSE)
      return(NULL)
    }
    return(list(data = z, time_info = vals))
  })
    # stop cluster
    parallel::stopCluster(cl)
    
    dts <- list(data = rbindlist(lapply(dts, '[[', 'data')),
                time_info = rbindlist(lapply(dts, '[[', 'time_info')))
    
    return(dts)
}

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

