#' calculate_variance
#'
#' @param dts
#' @param col_name
#'
#' @return
#' @export
#'
#' @examples
calculate_variance <- function(dts, col_name = "stokes") {
  dat <- get_data_table(dts)

  mean_dat <- dat[, .(mean_vals = mean(get(col_name))), by = distance]
  vals <- to_matrix(dts, col_name = col_name)

  fit <- lm.fit(y = vals, x = as.matrix(mean_dat$mean_vals))
  var(as.numeric(fit$residuals))
}
