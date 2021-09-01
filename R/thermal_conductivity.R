#' thermal_conductivity
#'
#' @param x 
#' @param power the input power to the heating cable
#'
#' @return
#' @export
#'
thermal_conductivity <- function(x, power) {
  (1.0 / x) * power / (4.0 * pi)
}