#' find_in_xml
#'
#' @param pattern 
#' @param search_string 
#'
#' @return
#' @export
#'
#' @examples
find_in_xml <- function(pattern, search_string) {
  grep(pattern, search_string, fixed = TRUE)
}

#' get_value_xml
#'
#' @param search_string 
#' @param pattern 
#'
#' @return
#' @export
#'
#' @examples
get_value_xml <- function(search_string, pattern = '(?<=>).*?(?=<)') {
  regmatches(search_string, regexpr(pattern, search_string, perl=TRUE))[[1]]
}
