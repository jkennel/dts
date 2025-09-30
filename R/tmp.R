# library(dts)
# library(data.table)
# library(ggplot2)
# library(viridis)
# library(hydrorecipes)
# 
# distance_ranges <- function(x, by = 'bath') {
#   get_distance_table(x)[, list(min_distance = min(distance),
#                                max_distance = max(distance)), by = list(type = get(by))]
# }
# 
# time_ranges <- function(x, by = 'type') {
#   get_time_table(x)[, list(min_time = min(start),
#                            max_time = max(start)), by = list(type = get(by))]
# }
# 
# x <- "../../../../mnt/c/Documents and Settings/jkennel/Documents/journal_articles/submissions/2024_dts_backfill/dts-data-analysis/data/2025-05-13 SEN7 A-DTS/2025-05-13 SEN7 MLS A-DTS Short Test 15 Min, 15w per m/channel 1"
# dbdir <- "../../../../mnt/c/Documents and Settings/jkennel/Documents/journal_articles/submissions/2024_dts_backfill/dts-data-analysis/dts_processed/sen_6_2025_05.duckdb"
# # dts <- dts:::to_duckdb(x, dbdir, n_cores = 20)
# dts <- dts:::load_duckdb(dbdir)
# dts <- find_water_bath(dts, buffer = 0.5)
# 
# bath_distance     <- dts$trace_distance[bath == TRUE]$distance
# non_bath_distance <- dts$trace_distance[bath == FALSE]$distance
# bath_dts <- dts$trace_data[distance %in% bath_distance]
# 
# bath_dts <- bath_dts[get_time_table(dts)[, list(start, calib_temperature)], on = 'start']
# bath_dts <- bath_dts[distance < 100]
# 
# dts <- bath_calibration(dts)
# 
# distance_range <- get_bath_limits(dts)
# 
# dts <- heating_time(dts,
#                     heating_type = "heating",
#                     distance_range = distance_range)
# dts <- heating_distance(dts,
#                         heating_type = "heating")
# # plot(heated~distance, get_distance_table(dts), type='l')
# half <- combine_by_symmetry(dts)
# 
# 
# 
# tr_time <- time_ranges(half, by = 'type')
# tr_time[, ymin := -Inf]
# tr_time[, ymax :=  Inf]
# 
# meta <- half$trace_time
# meta <- melt(meta[, list(start, probe_1, probe_2,
#                          reference_temperature,
#                          bath_temperature_dts)],
#              id.vars = 'start')
# meta[, value_adj := value - frollmean(value, 10), by = variable]
# 
# p <- ggplot() +
#   geom_rect(data = tr_time, aes(xmin = min_time,
#                                 xmax = max_time,
#                                 ymin = ymin,
#                                 ymax = ymax,
#                                 fill  = type),
#             color = 'transparent') +
#   scale_fill_manual(values = c('#fbc93d10', '#6cc0e510', '#fb4f4f10'),
#                     breaks = c('ambient', 'cooling', 'heating')) +
#   geom_line(data = meta, aes(x = start, y = value)) +
#   scale_x_datetime(expand = c(0,0)) +
#   facet_wrap(variable~., ncol = 1, scales = 'free_y') +
#   ylab('Temperature (°C)') +
#   theme_bw() +
#   theme(legend.position = 'top',
#         axis.title.x = element_blank())
# p
# 
# 
# 
# meta <- get_time_table(half)
# meta <- melt(meta[, list(start,
#                          probe_mean = calib_temperature,
#                          dts_bath_mean = bath_temperature_dts,
#                          calib_adj = -calib_adj,
#                          dts_adjusted = bath_temperature_dts - calib_adj)],
#              id.vars = 'start')
# 
# mean_t <- meta[, list(t_mean = paste('mean: ', round(mean(value, na.rm = TRUE),2), '(°C)'),
#                       start = min(start)), by = variable]
# 
# 
# 
# meta[, value_adj := value-mean(value, na.rm = TRUE), by = variable]
# p <- ggplot() +
#   geom_rect(data = tr_time, aes(xmin = min_time,
#                                 xmax = max_time,
#                                 ymin = ymin,
#                                 ymax = ymax,
#                                 fill  = type),
#             color = 'transparent') +
#   geom_label(data = mean_t, aes(x = start, y = Inf, label = t_mean),
#              color = viridis(1), size = 4, vjust = "inward", hjust = "inward",
#              label.padding = unit(0.1, units = 'lines')) +
#   scale_fill_manual(values = c('#fbc93d10', '#6cc0e510', '#fb4f4f10'),
#                     breaks = c('ambient', 'cooling', 'heating')) +
#   geom_line(data = meta, aes(x = start, y = value_adj)) +
#   scale_x_datetime(expand = c(0, 0))+
#   facet_wrap(variable~., ncol = 1) +
#   ylab('Temperature (°C)') +
#   theme_bw() +
#   theme(legend.position = 'top',
#         axis.title.x = element_blank())
# p
# 
# 
# sub_trace <- sample_times(half, n_traces = 3)
# tr <- distance_ranges(half, by = 'bath')
# tr[, ymin := -Inf]
# tr[, ymax :=  Inf]
# 
# 
# p <- ggplot() +
#   geom_rect(data = tr, aes(xmin = min_distance,
#                            xmax = max_distance,
#                            ymin = ymin,
#                            ymax = ymax,
#                            fill  = type),
#             color = 'transparent') +
#   geom_line(data = sub_trace, aes(x = distance, y = temperature)) +
#   coord_flip() +
#   scale_x_reverse(expand = c(0,0)) +
#   scale_fill_manual(values = c('#6cc0e510', '#fb4f4f10'),
#                     breaks = c(FALSE, TRUE)) +
#   facet_wrap(as.character(start)~., ncol = 5) +
#   theme_bw() +
#   xlab('Distance (m)') +
#   ylab('Temperature (°C)') +
#   theme(legend.position = 'top',
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(0.1, "lines"),
#         axis.text = element_text(size = 10),
#         strip.text = element_text(size = 10,
#                                   margin = margin(0,0,0,0, "cm")))
# 
# p
# 
# 
# sub_trace <- sample_distance(half, n_traces = 20)
# sub_trace <- sub_trace[start < as.POSIXct("2019-05-22 20:00:00", tz = "UTC")]
# p <- ggplot() +
#   geom_rect(data = tr_time, aes(xmin = min_time,
#                                 xmax = as.POSIXct("2019-05-22 20:00:00", tz = "UTC"),
#                                 ymin = ymin,
#                                 ymax = ymax,
#                                 fill  = type),
#             color = 'transparent') +
#   geom_line(data = sub_trace, aes(x = start, y = temperature)) +
#   scale_x_datetime(expand = c(0,0)) +
#   scale_fill_manual(values = c('#fbc93d10', '#6cc0e510', '#fb4f4f10'),
#                     breaks = c('ambient', 'cooling', 'heating')) +
#   facet_wrap(distance~., scales = 'free_y', ncol = 5) +
#   theme_bw() +
#   xlab('') +
#   ylab('Temperature (°C)') +
#   theme(legend.position = 'top',
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.spacing = unit(0.1, "lines"),
#         axis.text = element_text(size = 6),
#         axis.title.x = element_blank(),
#         strip.text = element_text(size = 4,
#                                   margin = margin(0,0,0,0, "cm")))
# 
# p
# 
# heat_times <- half[["trace_time"]][type == 'heating'][["start"]]
# start_heat <- heat_times[1]
# n_sub <- 3
# half_05 <- average_time(half, n_sub)
# half_05 <- subset_time(half_05, 0, start_heat + 3600 * 2)
# 
# library(hydrorecipes)
# x <- half_05
# time_var <- "start"
# trace_time <- get_time_table(x)
# types <- unique(trace_time[['type']])
# 
# heat_times <- trace_time[type == 'heating'][[time_var]]
# start_heat <- heat_times[1]-5
# end_heat   <- heat_times[length(heat_times)]
# 
# 
# tt <- (as.numeric(end_heat) - as.numeric(start_heat))
# dt <- diff(heat_times[1:2])
# 
# 
# di <- nrow(trace_time[type == 'heating'])
# 
# output <- subset_time(x, start_heat, end_heat)
# starting_vals <- get_temperature_breakpoint(x, start_heat)
# output$trace_data[starting_vals, temperature := temperature - temperature_0, on = 'distance']
# output <- to_matrix(output)
# output <- pad_output(output)
# 
# # input vector
# n <- nrow(output) / 2
# input <- rep(0.0, nrow(output))
# input[(n+5):(n+di + 1)] <- 1.0
# 
# dat <- data.table(input = as.numeric(input), output = output[,800])
# formula <- as.formula(output~.)
# h <- hydrorecipes::recipe(formula = formula, dat) |>
#   hydrorecipes::step_distributed_lag(input, 
#                                      knots = hydrorecipes:::log_lags(12, 2400)) |>
#   hydrorecipes::step_drop_columns(input) |>
#   step_ols(formula = formula) |>
#   hydrorecipes::prep() |>
#   hydrorecipes::bake()
# 
# 
# 
# 
# fits <- fit_convolve(half_05, n_knots = 18)
# plot(temperature[1]-cumulative_delta_temperature~elapsed_time, fits[distance == 105.219000], type = 'l',xlim = c(0, 1000), ylim = c(0, 13))
# points(temperature~elapsed_time, fits[distance == 105.219000], type = 'l', col = "red")
# 
# plot(-delta_temperature~elapsed_time, fits[distance == 105.219000], type = 'l',xlim = c(0, 100))
# 
