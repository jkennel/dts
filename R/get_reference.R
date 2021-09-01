get_reference <- function(x) {
  
  
  
  plot(temperature~distance, x$trace_data[start == x$trace_time$start[1]], type = 'l')
  plot(stokes~distance, x$trace_data[start == x$trace_time$start[1]], type = 'l')
  points(rev_stokes~distance, x$trace_data[start == x$trace_time$start[1]], type = 'l')
  
  plot(stokes~distance,
       x$trace_data[start == x$trace_time$start[1]], 
       type = 'l', xlim = c(-60, -50))
  points(rev_stokes~distance,
         x$trace_data[start == x$trace_time$start[1]],
         type = 'l', col = '#900000')
  
  n <- 1
  trim_start <- mean(-x[['channels']][channel_is_active == 1]$internal_fiber_length) + 0.3
  trim_end <- mean(x[['channels']][channel_is_active == 1]$measurement_length) + abs(trim_start)
  y1 <- subset_distance(x, trim_start, Inf)
  plot(stokes~distance, 
       y1$trace_data[start == y1$trace_time$start[n]], type = 'l',
       col = '#009000')
  points(rev(rev_stokes)~distance, 
         y1$trace_data[start == y1$trace_time$start[n]], type = 'l',
         col = '#00900060')
  
  points((anti_stokes)~distance, 
         y1$trace_data[start == y1$trace_time$start[n]], type = 'l',
         col = '#900000')
  points(rev(rev_anti_stokes)~distance, 
         y1$trace_data[start == y1$trace_time$start[n]], type = 'l',
         col = '#90000060')
  par(new = TRUE)
  plot(stokes/anti_stokes~distance, 
       y1$trace_data[start == y1$trace_time$start[n]], type = 'l',
       col = '#000090', 
       axes = FALSE, 
       ylim = c(1, 1.3))
  points(rev(rev_stokes/rev_anti_stokes)~distance, 
       y1$trace_data[start == y1$trace_time$start[n]], type = 'l',
       col = '#00009060', 
       axes = FALSE)
  
  points((rev(rev_stokes/rev_anti_stokes) + stokes/anti_stokes)/2~distance, 
         y1$trace_data[start == y1$trace_time$start[n]], type = 'l',
         col = '#000090', lwd = 2)
  
  abline(v = 0, lty = 2)
  abline(v = 207.85, lty = 2)
  abline(v = mean(y1$trace_distance$distance), lty = 2)
  
  
  plot((rev(rev_stokes/rev_anti_stokes) - stokes/anti_stokes)~distance, 
         y1$trace_data[start == y1$trace_time$start[n]], type = 'l',
         col = '#000090', lwd = 2, ylim = c(-0.03, 0.03))
  abline(h = -0.006)
  
  start_ref <- x[['device']][['ref_temp_start']]
  end_ref   <- x[['device']][['ref_temp_end']]
  len <- 207.850
  start_ref <- 37
  end_ref   <- 42
  sr <- max(x$trace_distance$distance)-55 - start_ref
  er <- max(x$trace_distance$distance)-55-end_ref
  y1 <- subset_distance(x, start_ref, end_ref)
  y2 <- subset_distance(x, er,  sr)
  
  plot(y2$trace_data$temperature~y2$trace_data$start)
  plot((y2$trace_data$rev_stokes)/y2$trace_data$rev_anti_stokes, pch = 20, cex = 0.2)
  plot((y1$trace_data$stokes)/y1$trace_data$anti_stokes, pch = 20, col = 'blue', cex = 0.2)
  points(((y2$trace_data$rev_stokes)/y2$trace_data$rev_anti_stokes + 
           (y1$trace_data$stokes)/y1$trace_data$anti_stokes +
           (y1$trace_data$rev_stokes)/y1$trace_data$rev_anti_stokes)/3, pch = 20, cex = 0.2, col = 'red')
  
  
  points(((y2$trace_data$rev_stokes)/y2$trace_data$rev_anti_stokes + 
            (y1$trace_data$stokes)/y1$trace_data$anti_stokes)/2, pch = 20, cex = 0.2, col = 'red')
  
  points((
            (y1$trace_data$stokes)/y1$trace_data$anti_stokes +
            (y1$trace_data$rev_stokes)/y1$trace_data$rev_anti_stokes)/2, pch = 20, cex = 0.2, col = 'red')
  
  
  plot((y2$trace_data$stokes)/y2$trace_data$anti_stokes, pch = 20, cex = 0.2)
  plot((y1$trace_data$rev_stokes)/y1$trace_data$rev_anti_stokes, pch = 20, cex = 0.2)
  
  mean((y1$trace_data$rev_stokes)/(y1$trace_data$stokes))
  mean((y2$trace_data$rev_stokes)/(y2$trace_data$stokes))
  
  range((y2$trace_data$rev_stokes))
  range((y1$trace_data$stokes))
  range((y1$trace_data$rev_stokes))
  
  mean((y2$trace_data$rev_stokes))
  mean((y1$trace_data$stokes))
  mean((y1$trace_data$rev_stokes))
  
  
  # ss <- min(x$trace_time$start)
  # ee <- min(x$trace_time$start) + 1500
  # y <- subset_time(y, begin = ss, end = ee)
  
  z <- y$trace_data[, list(mean_rat = mean(log(stokes/anti_stokes) +
                                           log((rev_stokes)/(rev_anti_stokes)))), by = start]
  
  plot(z$mean_rat)
  
  # image(to_matrix(y))
  plot((colMeans(to_matrix(y)[,1:72])))
  plot((colMeans(to_matrix(y)[,1:72])))
  plot(temperature~start, y$trace_data, cex = 0.5, pch = 20)
  
  y <- y$trace_data
  y <- y[, list(s1 = mean(stokes),
                s2 = mean(rev_stokes), 
                a1 = mean(anti_stokes),
                a2 = mean(rev_anti_stokes)), by = start]
  
  
  
  rat1 <- y$s1/y$a1
  rat2 <- y$s2/y$a2
  rng <- 700:1550
  plot(log(rat1)[rng], type='l')
  par(new = TRUE)
  plot(x$trace_time$ref_temperature[rng], type = 'l', col = 'red', ylim = c(-5, 30))
  
  dt <- data.table(ref = x$trace_time$ref_temperature[rng],
                   rat1 = log(rat1[rng]),
                   rat2 = log(rat2[rng])
                   )
  fit <- (lm(ref~rat1 + rat2, dt))
  
  plot(ref_temperature~start,x$trace_time, type = 'l', col = 'red', lwd = 2, 
       main = 'predicted reference (black) measured (red)')
  points(predict(fit, newdata = data.table(rat1 = log(rat1),
                                           rat2 = log(rat2)))~x$trace_time$start, type='l', lwd = 2)
  grid()
  
  plot(temperature~start, x$trace_data[distance == x$trace_distance[905]$distance][1:1700], type='l')
  points(probe_1~start,x$trace_time[1:1700], type = 'l', lwd = 2, 
       main = 'predicted reference (black) measured (red)', axes = FALSE, col = 'blue')
  
  par(new = TRUE)
  plot(ref_temperature[1:1700]~start,x$trace_time[1:1700], type = 'l', col = 'red', lwd = 2, 
       main = 'predicted reference (black) measured (red)', axes = FALSE)
  points(predict(fit, newdata = data.table(rat1 = log(rat1),
                                           rat2 = log(rat2)))[1:1700]~x$trace_time$start[1:1700], type='l', lwd = 2)
  grid()
  par(new = TRUE)
}
