#' thermal_conductivity
#'
#' @param x 
#' @param power 
#'
#' @return
#' @export
#'
#' @examples
thermal_conductivity <- function(x, power) {
  (1 / x) * power / (4 * pi)
}