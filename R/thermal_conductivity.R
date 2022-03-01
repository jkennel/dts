#' thermal_conductivity
#'
#' @param x the slope of the temperature curve
#' @param power the input power to the heating cable
#'
#' @return
#' @export
#'
thermal_conductivity <- function(x, power) {
  
  (1.0 / x) * (power / (4.0 * pi))

}