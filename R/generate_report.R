#' generate_report
#'
#' @param files_dir 
#' @param output_dir 
#'
#' @return
#' @export
#'
#' @examples
generate_report <- function(files_dir,
                            output_dir,
                            ...) UseMethod("generate_report")


#' @rdname generate_report
#' @export
generate_report.character <- function(files_dir,
                                      output_dir) {
  curwd <- getwd()
  
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  setwd(system.file('rmd/', package = 'dts'))


  tmp_dir <-  tempdir()
  fmt <- "bookdown::gitbook"
  cmd = sprintf("bookdown::render_book('index.Rmd', 'bookdown::gitbook',quiet = FALSE,params = list(dts_files = '%s'), new_session = TRUE,output_dir = '%s', clean = FALSE)",
                files_dir, tmp_dir)
  

  res = bookdown:::Rscript(c('-e', shQuote(cmd)))
  
  file.copy(from = tmp_dir,
            to = output_dir,
            recursive = TRUE,
            copy.date = TRUE)
  
  setwd(curwd)
}


#' @rdname generate_report
#' @export
generate_report.dts_long <- function(files_dir,
                                      output_dir) {
}