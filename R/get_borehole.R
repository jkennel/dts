# 
# 
# 
# 
# label_dts <- function(x) {
#   x <- dts# %>% combine_by_symmetry()
#   # sub <- x %>% average_time(n=5)
#   n_comp <- 10
#   x <- to_matrix(x)
#   a <- z
#   b <- svdr(a, n_comp)
#   tmp <- t(t(b$u[, 1:n_comp]))
#   # depth_labs <- as.numeric(cut(abs(1:nrow(a) - nrow(a)/2), breaks = 8)) * 0.0001
#   # tmp <- apply(cbind(tmp), 2, scale)
#   k <- kmeans(m, centers = 4, iter.max = 30, nstart = 300)
#   plot(k$cluster, pch = 20, cex = 0.5, ylab = 'groups')
#   par(new = TRUE)
#   plot(rowSums(a), type = 'l', col = '#00000050', axes = FALSE, ylab = '')
#   table(k$cluster)
#   
#   k2 <- kmeans(tmp[k$cluster == 2,], centers = 3, iter.max = 10, nstart = 300)
#   k3 <- kmeans(tmp[k$cluster == 1,], centers = 2, iter.max = 10, nstart = 300)
# 
#   plot(k2$cluster, pch = 20, cex = 0.5, ylab = 'groups')
#   plot(k3$cluster, pch = 20, cex = 0.5, ylab = 'groups')
#   # find heating times - label ambient, heating, cooling
#   bp <- time_breakpoints(x, shift = -20)
#   x$trace_time[, type := 'ambient']
#   x$trace_time[between(start, bp[1], bp[2]), type := 'heating']
#   x$trace_time[start >= bp[2], type := 'cooling']
#   
#   # set up distance table
#   distances <- x$trace_data[start == x$trace_time$start[1]][, list(distance)]
#   distances[, junction := FALSE]
#   distances[, heated := FALSE]
#   distances[, bath := FALSE]
#   distances[, borehole := FALSE]
#   setkey(distances, distance)
#   # find junction distances
# 
#   heat <- head(x$trace_time[type == 'heating'], 50)
#   cool <- head(x$trace_time[type == 'cooling'], 50)
#   
#   heat <- x$trace_data[heat[, list(start)], on = 'start']
#   cool <- x$trace_data[cool[, list(start)], on = 'start']
#   
#   heat <- to_matrix(heat)
#   cool <- to_matrix(cool)
#   
#   cor_sum <- cor_by_trace(heat, 1) + cor_by_trace(cool, 1)
#   n       <- length(cor_sum)
#   
#   junc    <- n/2-10 + which.min(cor_sum[(n/2 - 10):(n/2 + 10)])
#   distances[junc, junction := TRUE]
#   
#   
#   # find heated distances
#   
#   is_heated  <- range(which(cor_sum > 1.8))
#   distances[is_heated[1]:is_heated[2], heated := TRUE]
#   
#   # find water bath distances
#   std_dev <- x$trace_data[distances[heated != TRUE, list(distance)], on = 'distance']
#   std_dev <- std_dev[, list(sd_temp = sd(temperature)), by = distance]
#   std_dev[, sd_temp := sd_temp - min(sd_temp)]
#   std_dev <- std_dev[sd_temp < 0.05]
#   distances[std_dev, bath := TRUE, on = 'distance']
# 
#   
#   # find borehole distances
#   
#   heat <- tail(x$trace_time[type == 'ambient'], 200)
#   cool <- tail(x$trace_time[type == 'heating'], 200)
#   
#   heat <- x$trace_data[heat[, list(start)], on = 'start']
#   cool <- x$trace_data[cool[, list(start)], on = 'start']
#   
#   heat <- heat[distances[heated == TRUE, list(distance)], on = 'distance']
#   cool <- cool[distances[heated == TRUE, list(distance)], on = 'distance']
#   
#   
#   heat <- heat[, list(m_amb = mean(temperature)), by = distance]
#   cool <- cool[, list(m_heat = mean(temperature)), by = distance]
#   d <- heat[cool, on = 'distance']
#   d[, m_diff := m_heat - m_amb]
# 
#   
#   # get value near the junction
#   typical_difference <- d[distance %in% distances[junc-10:150]$distance, 
#                           list(mean_temp = mean(m_diff),
#                                sd_temp = sd(m_diff),
#                                med_temp = median(m_diff),
#                                mad_temp = mad(m_diff))]
#   diff_max <- typical_difference$mean_temp + typical_difference$sd_temp * 8
#   diff_min <- typical_difference$mean_temp - typical_difference$sd_temp * 8
#   d[, borehole := between(m_diff, diff_min, diff_max)]
# 
#   runs <- rle(d$borehole)
#   run_start <- runs$values[1]
#   runs$values[runs$lengths < 20] <- FALSE
#   runs <- rep(runs$values, times = runs$lengths)
#   runs <- range(which(runs))
#   distances[d[runs[1]:runs[2]], borehole := TRUE, on = 'distance']
#   
#   
#   plot(heated~distance, distances, col = 'red', ylim = c(1, 4), yaxt='n', ylab = '')
#   axis(2, at = c(1,2,3,4), labels = c('heated', 'bath', 'saturated', 'junction'))
#   points(2*bath~distance, distances, col = 'black')
#   points(3*borehole~distance, distances, col = 'blue')
#   points(4*junction~distance, distances, col = 'green')
#   
# 
#   
#   # variably saturated
#   heat <- head(x$trace_time[type == 'heating'], 100)
#   cool <- head(x$trace_time[type == 'cooling'], 100)
#   amb <- tail(x$trace_time[type == 'ambient'], 100)
#   
#   heat <- x$trace_data[heat[, list(start)], on = 'start']
#   cool <- x$trace_data[cool[, list(start)], on = 'start']
#   amb <- x$trace_data[amb[, list(start)], on = 'start']
#   
#   amb <- amb[, list(mean_temp = mean(temperature)), by = distance]
#   heat <- heat[distances[heated == TRUE, list(distance)], on = 'distance']
#   heat <- distances[borehole != TRUE, list(distance)][heat, on = 'distance', nomatch = 0]
# 
#   cool <- cool[distances[heated == TRUE, list(distance)], on = 'distance']
#   cool <- distances[borehole != TRUE, list(distance)][cool, on = 'distance', nomatch = 0]
#   
#   amb <- amb[distances[heated == TRUE, list(distance)], on = 'distance']
#   amb <- distances[borehole != TRUE, list(distance)][amb, on = 'distance', nomatch = 0]
# 
#   m <- t(to_matrix(heat))
#   elapsed_time <- 0:(nrow(m)-1)
#   fit_heat <- lm(m~elapsed_time)
#   m <- t(to_matrix(cool))
#   elapsed_time <- 0:(nrow(m)-1)
#   fit_cool <- lm(m~elapsed_time)
#   
#   depth_labs <- as.numeric(cut(abs(1:ncol(m) - ncol(m)/2), breaks = 40)) * 0.1
#   
#   clust_mat <- cbind(t(coefficients(fit_cool)),
#                      t(coefficients(fit_heat)), 
#                      depth_labs,
#                      amb$mean_temp)
#   clust_mat <- apply(clust_mat, 2, scale)
#   tmp <- kmeans(clust_mat,
#                 centers = 6,
#                 iter.max = 100,
#                 algorithm = "Hartigan-Wong")
#   table(as.integer(tmp$cluster))
#   plot(coefficients(fit_heat)[2,], type='p', col = as.integer(tmp$cluster))
#   plot(coefficients(fit_heat)[1,], type='p', col = as.integer(tmp$cluster))
#   
#   plot(coefficients(lm(m~elapsed_time))[1,], type='p', col = as.integer(tmp$cluster))
#   
#   m <- t(to_matrix(cool))
#   elapsed_time <- 0:(nrow(m)-1)
#   par(new = TRUE)
#   points(-coefficients(lm(m~elapsed_time))[2,500:700], type='l', col = 'red')
#   
#   
# }
