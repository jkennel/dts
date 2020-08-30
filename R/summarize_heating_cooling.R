#' summarize_heating_cooling
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
summarize_heating_cooling <- function(x) {
  
  x$trace_data[, list(sd_temp = sd(temperature),
                      mad_temp = mad(temperature),
                      mean_temp = mean(temperature),
                      median_temp = median(temperature),
                      min_temp = min(temperature),
                      max_temp = max(temperature),
                      q90 = quantile(temperature, 0.90),
                      q10 = quantile(temperature, 0.10),
                      diff_temp = diff(range(temperature)),
                      n = .N),
               by = list(distance, type)]
  
}







# sum_temp <- summarize_heating_cooling(x)
# plot_ly(sum_temp, x = ~distance, y = ~q10, color = ~type, 
#         type='scatter', mode = 'lines')

