#' plot_heatmap
#'
#' @param dts the dts set
#' @param trim_max maximum temperature values to set to NA
#' @param trim_min minimum temperature values to set to NA
#'
#' @return a plotly heatmap
#' @export
#'
plot_heatmap <- function(dts, trim_max = 120, trim_min = -5) {
  m <- to_matrix(dts, col_name = 'temperature')

  y_lab <- as.numeric(row.names(m))
  x_lab <- as.POSIXct(
    as.numeric(colnames(m)),
    origin = "1970-01-01",
    tz = "UTC"
  )

  m[m > trim_max] <- NA_real_
  m[m < trim_min] <- NA_real_

  plot_ly(
    z = m,
    x = x_lab,
    y = y_lab,
    colors = viridis(100),
    type = "heatmap"
  )
}


#' plot_distances
#'
#' @param dts the dts set
#' @param n number of traces to plot
#' @param trim_max maximum temperature values to set to NA
#' @param trim_min minimum temperature values to set to NA
#'
#' @return a plotly heatmap
#' @export
#'
plot_distances <- function(
  dts,
  n = 10,
  trim_max = 120,
  trim_min = -5,
  log_x = FALSE
) {
  dts <- sample_distance(dts, n)
  dat <- get_data_table(dts)

  dat[temperature > trim_max, temperature := NA_real_]
  dat[temperature < trim_min, temperature := NA_real_]

  s <- ggplot2::scale_x_datetime()
  if (log_x) {
    dat[, start := as.numeric(start) - as.numeric(start)[1]]
    s <- ggplot2::scale_x_log10()
  }

  plotly::ggplotly(
    ggplot2::ggplot(dat, aes(x = start, y = temperature)) +
      geom_line() +
      s +
      facet_wrap(distance ~ ., scales = "free_y") +
      theme_bw()
  )
}


#' plot_times
#'
#' @param dts the dts set
#' @param n number of traces to plot
#' @param trim_max maximum temperature values to set to NA
#' @param trim_min minimum temperature values to set to NA
#'
#' @return a plotly heatmap
#' @export
#'
plot_times <- function(dts, n = 10, trim_max = 120, trim_min = -5) {
  dts <- sample_times(dts, n)
  dat <- get_data_table(dts)

  dat[temperature > trim_max, temperature := NA_real_]
  dat[temperature < trim_min, temperature := NA_real_]

  plotly::ggplotly(
    ggplot2::ggplot(dat, aes(x = distance, y = temperature)) +
      geom_line() +
      facet_wrap(start ~ ., scales = "free_y") +
      theme_bw()
  )
}
