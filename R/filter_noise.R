#' filter_noise
#'
#' @param x data read from a dts
#' @param n number of samples
#'
#' @return
#' @rdname filter_noise
#' @export
#'
#' @examples
filter_noise <- function(x, n) UseMethod("filter_noise")


#' @rdname filter_noise
#' @export
filter_noise.numeric <- function(x, n) {
  
  x - aquifer::fftw_convolve(x, waterlevel::window_nuttall(n))
  
}


#' @rdname filter_noise
#' @export
filter_noise.data.table <- function(x, n) {
  
  x[, list(temperature_adj = filter_noise(temperature, 3), 
           start), by = distance]
  
}

# plot(filter_noise(rnorm(100), 3))

# tmp <- filter_noise(lng$trace_data, 61)
# 
# 
# plot(temperature_adj~start, tmp[distance == 110.406000], type='l')

