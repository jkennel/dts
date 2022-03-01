pad_input <- function(x) {
  c(rep(0, length(x)), x)
}

#' pad_output
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
pad_output <- function(x) {
  pad <- matrix(0.0, nrow = nrow(x), ncol = ncol(x))
  t(cbind(pad, x))
}


#' fit_convolve
#'
#' @param x 
#' @param n_knots 
#' @param cool_mult 
#' @param time_var 
#'
#' @return
#' @export
#'
#' @examples
fit_convolve <- function(x,
                         cool_mult = 0.5,
                         time_var = 'start',
                         n_knots = NULL 
                         ) {
  
x <- copy(dts)
  # subset dates
  trace_time <- get_time_table(x)
  types <- unique(trace_time[['type']])
    
  heat_times <- trace_time[type == 'heating'][[time_var]]
  start_heat <- heat_times[1]
  end_heat <- heat_times[length(heat_times)]
  
  
  tt <- (as.numeric(end_heat) - as.numeric(start_heat))
  dt <- diff(heat_times[1:2])
  if(is.null(n_knots)) {
    n_knots <- 2#(tt %/% 600) 
  }
  
  
  di <- nrow(trace_time[type == 'heating'])
  
  output <- subset_time(x, start_heat, end_heat)
  starting_vals <- get_temperature_breakpoint(x, start_heat)
  output$trace_data[starting_vals, temperature := temperature - temperature_0, on = 'distance']
  output <- to_matrix(output)
  output <- pad_output(output)
  
  # input vector
  n <- nrow(output) / 2
  input <- rep(0.0, nrow(output))
  input[(n):(n+di)] <- 1.0
  
  knots <- c(0:5, 6 + waterlevel::log_lags(n_knots, n-6))
  #knots <- waterlevel::log_lags(n_knots, n)
  
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
  # coefs  <- solve_arma(dl, output)
  
  pdf()
  for(nn in 1300:1800) {
    
  # nn <- 1502

  coefs <- (as.matrix(coefficients(
    cv.glmnet(dl, output[, nn], 
              lower.limits = 0, 
              upper.limits = c(0, rep(Inf, length(knots) - 1)), 
              intercept = FALSE,
              family = 'gaussian',
              nfolds = 10,
              lambda = 10^(seq(-3, -1, 0.01)),
              relax = TRUE,
              alpha = 0))[-1]))

  out <- bl %*% coefs
  
  et <- trace_time[type == 'heating']$elapsed_time
  
  plot(c(log(et), NA), (output[, nn]), col = 'red', ylim = c(0, 8), pch = 20, cex = 0.5)
  points(log(et), cumsum(out), type = 'l')
  # plot(output[, nn] - cumsum(out), type = 'l', ylim = c(-1, 1))
  # abline(h = 0)
  
  # points((et), (7.5 / (4.0 * pi)) * c(NA,diff(log(et))) / pmax(out, 1e-4), type = 'l',
  #        ylim = c(0,20), col = 'black')
  
  }
  dev.off()
  plot((et), (7.5 / (4.0 * pi)) * c(NA,diff(log(et))) / pmax(out, 1e-4), type = 'l',
         ylim = c(0,20), col = 'black')
  abline(h = 2, lty = 2, col = 'blue')
  
  plot(log(et), c(NA,diff(log(et))) / pmax(out, 1e-4), type = 'l', ylim = c(0,20))
  
  plot(c(log(et), NA), (output[, nn]), col = 'red')
  points(log(et), cumsum(out), type = 'l')
  
  
  
  
  # output matrix
  output <- subset_time(x, start_heat, start_cool + dt * cool_mult)
  output$trace_data[starting_vals, temperature := temperature - temperature_0, on = 'distance']
  output <- to_matrix(output)
  output <- pad_output(output)
  
  
  start_cool <- trace_time[type=='cooling'][[type]][1]
  dt <- as.numeric(start_cool) - as.numeric(start_heat)
  
  
  starting_vals <- get_temperature_breakpoint(x, start_heat)
  
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

