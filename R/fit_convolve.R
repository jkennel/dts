pad_input <- function(x) {
  c(rep(0, length(x)), x)
}

pad_output <- function(x) {
  pad <- matrix(0.0, nrow = nrow(x), ncol = ncol(x))
  t(cbind(pad, x))
}


#' fit_convolve
#'
#' @param x 
#' @param n_knots 
#' @param cool_mult 
#'
#' @return
#' @export
#'
#' @examples
fit_convolve <- function(x,
                         n_knots = 10, 
                         cool_mult = 0.5) {
  
  # x <- copy(dts)
  # x <- average_time(x, 5)
  # subset dates
  trace_time <- get_time_table(x)
  start_heat <- trace_time[type=='heating']$start[1]
  start_cool <- trace_time[type=='cooling']$start[1]
  di <- nrow(trace_time[type=='heating'])
  dt <- as.numeric(start_cool) - as.numeric(start_heat)
  
  
  starting_vals <- get_temperature_breakpoint(x, start_heat)
  
  # output matrix
  output <- subset_time(x, start_heat, start_cool + dt * cool_mult)
  output$trace_data[starting_vals, temperature := temperature - temperature_0, on = 'distance']
  output <- to_matrix(output)
  output <- pad_output(output)
  
  # input vector
  n <- nrow(output) / 2
  input <- rep(0.0, nrow(output))
  input[(n):(n+di)] <- 1.0

  knots <- waterlevel::log_lags(n_knots, n)
  
  # generate distributed lags
  dl <- waterlevel::distributed_lag(x = input, 
                                    knots = knots, 
                                    spline_fun = splines::ns,
                                    lag_name = '',
                                    n_subset = 1, 
                                    n_shift = 0)
  
  bl <- splines::ns(min(knots):max(knots), 
                    knots = knots[-c(1, length(knots))],
                    Boundary.knots = c(min(knots), max(knots)),
                    intercept = TRUE)
  
  wh <- which(is.na(dl[,1]))
  dl <- dl[-wh,]
  output <- output[-wh,]
  coefs  <- solve_arma(dl[-(1:120),], output[-(1:120),])

  out <- bl %*% coefs

  out
  
}

