# n <- 400:400
# trim_start <- 0#mean(-x[['channels']][channel_is_active == 1]$internal_fiber_length) + 0.3
# trim_end   <- mean(x[['channels']][channel_is_active == 1]$measurement_length)# + abs(trim_start)
# y1 <- subset_distance(x, trim_start, trim_end)
# y1 <- y1$trace_data[start %in% y1$trace_time$start[n]]
# y1 <- y1[, list(stokes = mean(stokes), 
#           anti_stokes = mean(anti_stokes),
#           rev_stokes = mean(rev_stokes),
#           rev_anti_stokes = mean(rev_anti_stokes),
#           temperature = mean(temperature)), by = list(distance)]
# 
# 
# 
# y1[, rev_stokes := (rev_stokes + (mean(stokes) - mean(rev_stokes))) ]#* (mean(stokes)/mean(rev_stokes)))]
# y1[, rev_anti_stokes := (rev_anti_stokes  + (mean(anti_stokes) - mean(rev_anti_stokes))) ]#* (mean(anti_stokes)/mean(rev_anti_stokes)))]
# 
# tmp <- loss_profile(y1)
# 
# plot(-cumsum(tmp[1:817]), type = 'l')
# points(rev(cumsum(tmp[817:1])), type='l')
# 
# points((rev(cumsum(tmp[817:1])) - cumsum(tmp[1:817]))/2, type='l')
# plot(temperature~distance, y1, type='l')
# 
# 
# y1[, stokes_cor := (stokes+rev_stokes)/2]
# y1[, anti_stokes_cor := (anti_stokes+rev_anti_stokes)/2]
# 
# # plot(stokes_cor~distance, y1, type='l')
# # abline(v = c(mean(x[['channels']][channel_is_active == 1]$measurement_length) + c(5,25), c(-5, -25)), lty = 2)
# # plot(temperature~distance, y1, type='l',ylim =  c(5, 11), col = '#0050FF40', lwd = 2)
# # par(new = TRUE)
# # plot(log(stokes_cor/anti_stokes_cor)~distance, y1, type='l', ylim = c(0.105, 0.14))
# # abline(v = c(mean(x[['channels']][channel_is_active == 1]$measurement_length) + c(5,20), c(-5, -20)), lty = 2, col = 'blue')
# # abline(v = c(mean(x[['channels']][channel_is_active == 1]$measurement_length) - c(7.3,11.5), c(7.3,11.5)), lty = 2, col = 'red')
# # abline(h = c(0.108), col = '#00000040')
# # 
# # plot(log(stokes_cor/anti_stokes_cor)~temperature, y1, type='p', pch = 20)
# 
# plot(residuals(lm(log(stokes_cor/anti_stokes_cor)~temperature, y1)))
# 
# # y1[, rev_stokes := rev(rev_stokes)]
# # y1[, rev_anti_stokes := rev(rev_anti_stokes)]
# # plot(y1$stokes, type='l')
# # points(y1$rev_stokes, type='l')
# # plot(runmean(loss_profile(y1, 0), 21), type='l',ylim = c(-0.0001, 0.0001))
# loss_profile(y1)
# y1[, diff := (log(stokes/anti_stokes) + rev(log(rev_stokes/rev_anti_stokes)))/2]
# plot(log(stokes/anti_stokes)~distance, y1, type='l', ylim = c(0.062, 0.15), col = '#00009050', xlim = c(0, 208))
# points(rev(log(rev_stokes/rev_anti_stokes))~distance, y1, type='l', ylim = c(0.1, 0.15), col = '#00009050')
# points(diff~distance , y1, type='l', ylim = c(0.1, 0.15), col = '#00009050', lwd = 0.1)
# points((diff+rev(diff))/2~distance , y1, type='l', ylim = c(0.1, 0.15), col = 'red', lwd = 2)
# par(new = TRUE)
# plot(temperature~distance, y1, type='l', ylim = c(22, 5), lwd = 2, xlim = c(0, 208))
# 
# loss_profile <- function(x, dx) {
#   i1 <- log(x$stokes/x$anti_stokes)
#   i2 <- log(x$rev_stokes/x$rev_anti_stokes)
#   alpha <- c(-0.5 * central_difference(i1 - i2, 1))
#   alpha[is.nan(alpha)] <- 0
#   alpha[is.na(alpha)] <- 0
#   # plot(((alpha))~y1$distance, type = 'l', ylim = c(-0.0001, 0.0001))
#   alpha
# }
# 
# central_difference <- function(x, dx) {
#   x_p1 <- c(x[-1], NA_real_)
#   x_m1 <- c(NA_real_, x[-length(x)])
#   
#   (x_p1 - x_m1) / (2*dx)
#   
# }
