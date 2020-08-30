# library(dts)
# library(plotly)
# library(data.table)
# library(aquifer)
# library(waterlevel)
# library(viridis)
# library(roll)
# 
# 
# dts <- readRDS('/media/kennel/Seagate Expansion Drive/dts/gdc_05_2015-11-02/dts.rds')
# x <- dts$trace_data
# s <- unique(x$start)[500]
# plot(temperature~distance, x[start == s], type='l')
# y <- dts$trace_time
# n <- 601
# setkey(x, start)
# x[, temp_adj := temperature - fftw_convolve(x = temperature, y = waterlevel::window_nuttall(n)), by = distance]
# x[, temperature := NULL]
# x <- na.omit(x)
# x <- dcast(x, distance~start, value.var = 'temp_adj')
# x[, distance := NULL]
# x <- t(as.matrix(x))
# 
# x <- cor(x)
# 
# p <- plot_ly(z = x,
#                colors = viridis(100),
#                type = "heatmap", height = 700) 
# p
