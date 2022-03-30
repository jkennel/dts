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
#' @param x the dts object
#' @param n_knots number late time knots in lag model - generally keep small
#' @param cool_mult currently not used
#' @param time_var column name of the that contains the date time info
#'
#' @return table of slopes and responses
#' @export
#'
#' @examples
fit_convolve <- function(x,
                         cool_mult = 0.5,
                         time_var = 'start',
                         n_knots = NULL
) {
  

  trace_time <- get_time_table(x)
  types <- unique(trace_time[['type']])
  
  heat_times <- trace_time[type == 'heating'][[time_var]]
  start_heat <- heat_times[1]
  end_heat <- heat_times[length(heat_times)]
  
  
  tt <- (as.numeric(end_heat) - as.numeric(start_heat))
  dt <- diff(heat_times[1:2])
  
  if(is.null(n_knots)) {
    n_knots <- 12
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
  
  knots <- c(hydrorecipes::log_lags(n_knots, n))
  

  
  bl <- splines::ns(min(knots):max(knots), 
                    knots = knots[-c(1, length(knots))],
                    Boundary.knots = c(min(knots), max(knots)),
                    intercept = TRUE)
  
  # generate distributed lags
  dl <- hydrorecipes::distributed_lag(x = input, 
                                      basis_mat = bl, 
                                      knots = knots)
  
  wh <- which(is.na(dl[, 1]))
  dl <- dl[-wh, ]
  output <- output[-c(wh), , drop = FALSE]
  dat <- list()
  for(i in 1:ncol(output)) {
    coefs <- (as.matrix(coefficients(
      glmnet::cv.glmnet(dl, output[, i], 
                lower.limits = 0, 
                upper.limits = c(rep(Inf, length(knots))), 
                intercept = TRUE,
                family = 'gaussian',
                nfolds = 10,
                lambda = 10^(seq(-3, -1.5, 0.1)),
                relax = TRUE,
                alpha = 0))))
    
    out <- (bl %*% coefs[-1]) 
    out[1] <- out[1] + coefs[1]
    et <- trace_time[type == 'heating']$elapsed_time


    dat[[i]] <- data.table(elapsed_time = et,
                           elapsed_time_log = log(et),
                           delta_time_log = c(diff(log(et)), NA),
                           delta_temperature = as.numeric(out)[-1], 
                           cumulative_delta_temperature = cumsum(as.numeric(out))[-1], 
                           distance = as.numeric(colnames(output)[[i]]),
                           temperature = as.numeric(output[,i]))
  }
  
  
  rbindlist(dat)
  
}


# x <- subset_distance(dts, begin = 80, end = 130)
# fc <- fit_convolve(x)
# 
# pdf()
# # fc[, plot(pmax(delta_temperature/elapsed_time_log, 1e-4)~elapsed_time, ylim = c(1e-4, 1), log = 'xy', type = 'l', main = distance), by = distance]
# fc[, plot(temperature~elapsed_time, ylim = c(0,10), log = 'x', type = 'l', main = distance), by = distance]
# dev.off()
# 
# # fc[data.table(depth = 40:50), on = 'depth']
# 
# # dev.off()
# 
# 
# plot(diff(cumsum(out)) / diff(et), type = 'l', log = 'xy')
# 
# plot(x = c(log(et), NA), y = as.numeric((output[, i])), col = 'red', pch = 20, cex = 0.5)
# points(log(et), cumsum(out), type = 'l')
# plot(output[, i] - cumsum(out), type = 'l', ylim = c(-1, 1))

# abline(h = 0)

# points((et), (7.5 / (4.0 * pi)) * c(NA,diff(log(et))) / pmax(out, 1e-4), type = 'l',
#        ylim = c(0,20), col = 'black')

# plot((et), (7.5 / (4.0 * pi)) * c(NA,diff(log(et))) / pmax(out, 1e-4), type = 'l',
#        ylim = c(0,20), col = 'black')
#   abline(h = 2, lty = 2, col = 'blue')
#   
#   plot(log(et), c(NA,diff(log(et))) / pmax(out, 1e-4), type = 'l', ylim = c(0,20))
#   
#   plot(c(log(et), NA), (output[, nn]), col = 'red')
#   points(log(et), cumsum(out), type = 'l')
#   
#   
#   
#   
#   # output matrix
#   output <- subset_time(x, start_heat, start_cool + dt * cool_mult)
#   output$trace_data[starting_vals, temperature := temperature - temperature_0, on = 'distance']
#   output <- to_matrix(output)
#   output <- pad_output(output)
#   
#   
#   start_cool <- trace_time[type=='cooling'][[type]][1]
#   dt <- as.numeric(start_cool) - as.numeric(start_heat)
#   
#   
#   starting_vals <- get_temperature_breakpoint(x, start_heat)
#   
#   # input vector
#   n <- nrow(output) / 2
#   input <- rep(0.0, nrow(output))
#   input[(n):(n+di)] <- 1.0
# 
#   knots <- waterlevel::log_lags(n_knots, n)
#   
#   # generate distributed lags
#   dl <- waterlevel::distributed_lag(x = input, 
#                                     knots = knots, 
#                                     spline_fun = splines::ns,
#                                     lag_name = '',
#                                     n_subset = 1, 
#                                     n_shift = 0)
#   
#   bl <- splines::ns(min(knots):max(knots), 
#                     knots = knots[-c(1, length(knots))],
#                     Boundary.knots = c(min(knots), max(knots)),
#                     intercept = TRUE)
#   
#   wh <- which(is.na(dl[,1]))
#   dl <- dl[-wh,]
#   output <- output[-wh,]
#   coefs  <- solve_arma(dl[-(1:120),], output[-(1:120),])
# 
#   out <- bl %*% coefs
# 
#   out
#   
# }

