#' generate_report
#'
#' @param input folder or dts data already parsed
#' @param output_dir 
#' @param fmt 
#' @param ... heating_power, heating_type
#'
#' @return
#' @export
#'
#' @examples
generate_report <- function(input,
                            output_dir,
                            fmt = 'bookdown::gitbook',
                            ...) UseMethod("generate_report")


#' @rdname generate_report
#' @export
generate_report.character <- function(input,
                                      output_dir, 
                                      fmt = 'bookdown::gitbook',
                                      n_cores = 1,
                                      ...) {
  
  dts <- read_dts_xml(input, n_cores = n_cores) |>
    dts_to_long()
  
  generate_report(dts, output_dir, fmt, dir_name = input, ...)

}



#' @rdname generate_report
#' @export
generate_report.dts_long <- function(input,
                                     output_dir, 
                                     fmt = 'bookdown::gitbook',
                                     dir_name,
                                     ...) {
  

  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }

  curwd <- getwd()
  
  args <- list(...)
  args <- append(list(input = input),
                 args)
  

  tmp_dir <-  paste0(tempdir(), '/dts_', format(Sys.time(), format = '%Y%m%d%H%M%S'))
  
  
  # generate temporary directory
  setwd(system.file('rmd/', package = 'dts'))
  
  bookdown::render_book(
    input         = 'index.Rmd', 
    output_format = fmt, 
    quiet         = FALSE,
    params        = args, 
    new_session   = TRUE,
    output_dir    = tmp_dir, clean = FALSE)
  
  
  file.copy(from = tmp_dir,
            to = output_dir,
            recursive = TRUE,
            copy.mode = FALSE,
            copy.date = TRUE)
  
  setwd(curwd)
  
  return('Book generated successfully. You Rock!')
  
}




