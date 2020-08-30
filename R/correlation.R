
cor_distance <- function(x, n=601, max_size = 3000) {
  
  z <- copy(x)
  z[, temp_adj := temperature - aquifer::fftw_convolve(x = temperature, 
                                                       y = waterlevel::window_nuttall(n)), by = distance]
  
  z[, temperature := NULL]
  z <- na.omit(z)
  
  z <- dcast(z, distance~start, value.var = 'temp_adj')
  if(nrow(z) > max_size) {
    z <- z[seq(1, nrow(z), length.out = max_size)]  
  }
  
  z[, distance := NULL]
  
  z <- t(as.matrix(z))
  
  z <- cor(z)
  
}
 
cor_time_line <- function(x, n=21, max_size = 3000) {
  
  z <- copy(x)
  z[, temp_adj := temperature - aquifer::fftw_convolve(x = temperature, 
                                                       y = waterlevel::window_nuttall(n)), by = start]
  
  z[, temperature := NULL]
  z <- na.omit(z)
  
  z <- dcast(z, start~distance, value.var = 'temp_adj')
  
  z[, start := NULL]
  
  z <- t(as.matrix(z))
  
  z_cor <- roll_cor(z, width = 2)
  
}

cor_time <- function(x, n=21, max_size = 3000) {
  
  z <- copy(x)
  z[, temp_adj := temperature - aquifer::fftw_convolve(x = temperature, 
                                                       y = waterlevel::window_nuttall(n)), by = start]
  
  z[, temperature := NULL]
  z <- na.omit(z)
  
  z <- dcast(z, start~distance, value.var = 'temp_adj')
  
  if(nrow(z) > max_size) {
    z <- z[seq(1, nrow(z), length.out = max_size)]  
  }
  
  z[, start := NULL]
  
  z <- t(as.matrix(z))
  
  z <- cor(z)
  
}

# 
# ax <- list(
#   title = "",
#   zeroline = FALSE,
#   showline = FALSE,
#   showticklabels = FALSE,
#   showgrid = FALSE
# )
# 
# tms <- sort(unique(y$start))
# n <- 300
# sub_start <- seq(1, length(tms)-n, n)
# z <- list()
# p <- list()
# for(i in seq_along(sub_start)) {
#   z[[i]] <- cor_time(x[start %in% tms[sub_start[i]:(sub_start[i] + n-1)]], max_size = 2000)
#   if(i == 1) {
#     sl <- TRUE
#   } else {
#     sl <- FALSE
#   }
#   p[[i]] <- plot_ly(z = z[[i]],
#                     colors = viridis(20),
#                     type = "heatmap",
#                     zmin = 0,
#                     zmax = 1) %>% 
#     hide_colorbar %>%
#     layout(xaxis = ax, yaxis = ax)
# }
# 
# subplot(p, nrows = floor(sqrt(length(sub_start))), margin = 0.01) 
# 
# 
# p <- plot_ly(z = cor_time(x1, max_size = 2000),
#              colors = viridis(100),
#              type = "heatmap", height = 700) 
# p
# 
# p <- plot_ly(z = cor_distance(x),
#              colors = viridis(100),
#              type = "heatmap", height = 700) 
# p

