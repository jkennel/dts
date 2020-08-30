#' fit_heating_cooling
#'
#' @param x 
#' @param start 
#' @param end 
#'
#' @return
#' @export
#'
#' @examples
fit_heating_cooling <- function(x, start_time = 0.5, end_time = 86400) {
  
  # x <- copy(x)
  # bp <- time_breakpoints(x, shift = -20, col_name = 'temperature')
  
  heat <- get_time_type(x, time_type = 'heating')
  cool <- get_time_type(x, time_type = 'cooling')

  heat = prep_fit(heat, start_time, end_time)
  cool = prep_fit(cool, start_time, end_time)
  
  heat[, type := 'heating']
  cool[, type := 'cooling']
  
  rbindlist(list(heat, cool))
  
}

prep_fit <- function(x, start_time, end_time) {
  
  # x[, elapsed_time := as.numeric(start-start[1]), by = distance]
  x <- get_data_table(x)[between(elapsed_time, start_time, end_time)]
  
  output <- t(to_matrix(x))
  input  <- log(unique(x[['elapsed_time']]))
  fit    <- lm(output~input)
  
  out <- data.table(distance = unique(x[['distance']]))
  out <- cbind(out, add_fit_columns(fit))
  
  out
  
}


param_summary <- function(x) {
  
  c(r2 = x$r.squared,
    sigma = x$sigma,
    slope = x$coefficients[2,1],
    se_slope = x$coefficients[2,2]
    )
  
}

add_fit_columns <- function(x) {
  
  sum_fit <- summary(x)
  as.data.table(t(vapply(sum_fit, param_summary, FUN.VALUE =  numeric(4))))

}





# 
# 
# pad_input_output <- function(x) {
#   
#   bp <- time_breakpoints(x,
#                          shift = -20, 
#                          col_name = 'temperature_adj')
#   
#   wh1 <- which(x$trace_time$start == bp[[1]])
#   wh2 <- which(x$trace_time$start == bp[[2]])
#   dt <- as.numeric(bp[[2]])-as.numeric(bp[[1]])
#   
#   s <- bp[[1]]
#   e <- bp[[1]] + 2.6 * dt
#   
#   test_dat <- x$trace_data[between(start, s, e)]
#   test_dat <- test_dat[distance == 94.594200]
#   #test_dat <- test_dat[distance == 126.090000]
#   test_dat[, temperature_adj := temperature_adj-temperature_adj[1]]
#   test_dat[, start_adj := start-start[1]]
#   n <- nrow(test_dat)
#   
#   pad <- c(rep(0, nrow(test_dat)), test_dat$temperature_adj)
#   n_pad <- length(pad)
#   
#   rate <- c(rep(0, nrow(test_dat)), rep(1, wh2-wh1+1))
#   n_rate <- length(rate)
#   
#   rate <- c(rate, rep(0, n_pad - n_rate))
#   
#   dat <- data.table(temp = pad, rate = rate)
# }
#   
# 
# 
# extend_test <- function(x) {
#   
#   bp <- time_breakpoints(x, shift = -20, col_name = 'temperature_adj')
#   
#   wh1 <- which(x$trace_time$start == bp[[1]])
#   wh2 <- which(x$trace_time$start == bp[[2]])
#   dt <- as.numeric(bp[[2]])-as.numeric(bp[[1]])
#   
#   s <- bp[[1]]
#   e <- bp[[2]] + dt
#   
#   test_dat <- x$trace_data[between(start, s, e)]
#   test_dat <- test_dat[distance == 94.594200]
#   #test_dat <- test_dat[distance == 126.090000]
#   test_dat[, temperature_adj := temperature_adj-temperature_adj[1]]
#   test_dat[, start_adj := start-start[1]]
#   n <- nrow(test_dat)
#   
#   pad <- c(rep(0, nrow(test_dat)), test_dat$temperature_adj)
#   n_pad <- length(pad)
#   
#   rate <- c(rep(0, nrow(test_dat)), rep(1, wh2-wh1+1))
#   n_rate <- length(rate)
#   
#   rate <- c(rate, rep(0, n_pad - n_rate))
#   
#   dat <- data.table(temp = pad, rate = rate)
#   rec <- recipe(temp~., dat) %>%
#     step_distributed_lag(rate, knots = log_lags(10, 10000)) %>%
#     prep() %>%
#     portion()
# 
#   fit_dist <- lm(outcome ~ distributed_lag, 
#                  data = rec,
#                  tol = 1e-50,
#                  x = FALSE, y = FALSE,
#                  na.action = na.exclude)
#   d <- response_from_fit(fit_dist)
#   
#   plot((d[type == 'cumulative_response']$value), type='l', col = 'red', log = 'x')
#   
# }
# 
# fit <- fit_heating_cooling(half)
