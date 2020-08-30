#' #' kmeans_distance
#' #'
#' #' @param x 
#' #' @param n_comp 
#' #' @param ... 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' 
#' kmeans_distance <- function(x, ...) UseMethod("kmeans_distance")
#' 
#' 
#' 
#' #' @rdname kmeans_distance
#' #' @export
#' kmeans_distance.dts_long <- function(x, n_comp = 10, ...) {
#'   
#'   
#'   kmeans_distance(to_matrix(x), n_comp = n_comp)
#'   
#'   
#' }
#' 
#' #' @rdname kmeans_distance
#' #' @export
#' kmeans_distance.matrix <- function(x, n_comp = 5, ...) {
#'   
#'   
#'   # find heating times - label ambient, heating, cooling
#'   m <- to_matrix(x)
#'   
#'   bp <- time_breakpoints(x, shift = -20)
#'   x$trace_time[, type := 'ambient']
#'   x$trace_time[between(start, bp[1], bp[2]), type := 'heating']
#'   x$trace_time[start >= bp[2], type := 'cooling']
#'   
#'   
#'   # svd kmeans
#'   m <- apply(m, 2, scale, scale = FALSE)
#'   m <- svdr(x, n_comp)$u
#'   
#'   # initial clusters
#'   k <- kmeans(m, centers = 4, iter.max = 30, nstart = 300)
#'   
#'   
#' }



#' calibration_bath
#'
#' @param x 
#' @param n_comp 
#'
#' @return
#' @export
#'
#' @examples
calibration_bath <- function(x, n_comp = 2) {
  
  distances <- x$trace_distance
  
  # find water bath distances
  std_dev <- get_data_table(x)[distances[heated != TRUE, list(distance)], on = 'distance']
  std_dev <- std_dev[, list(sd_temp = sd(temperature)), by = distance]
  std_dev[, sd_temp := sd_temp - min(sd_temp)]
  std_dev <- std_dev[sd_temp < 0.05]
  x$trace_distance[std_dev, bath := TRUE, on = 'distance']
  
  invisible(x)
  
}

#' heated_groups
#'
#' @param x 
#' @param n_comp 
#' @param n_buffer 
#'
#' @return
#' @export
#'
#' @examples
heated_groups <- function(x, n_comp = 5, n_clust = 5,  n_buffer = 1000) {
  

  times <- x$trace_time[type == 'heating'][1]$start
  s <- times - n_buffer
  e <- times + n_buffer
  heated <- to_matrix(x$trace_data[between(start, s, e)])
  
  times <- x$trace_time[type == 'cooling'][1]$start
  s <- times - n_buffer
  e <- times + n_buffer
  cooling <- to_matrix(x$trace_data[between(start, s, e)])

  # times <- x$trace_time[type == 'ambient'][1]$start
  # s <- times
  # e <- times + 200
  # ambient <- to_matrix(x$trace_data[between(start, s, e)])
  # 
  
  
  s <- svdr(cbind(heated, cooling), n_comp)$u
  # a <- kmeans(s, centers = 3, iter.max = 30, nstart = 300)
  a <- Mclust(s, G = n_clust)

  x$trace_distance[, classification := a$classification]

  x
  # 
  # tmp <- x$trace_data[distances, on = 'distance']
  # tmp[x$trace_time[type=='ambient'], 
  #     list(sd_temp = sd(temperature),
  #          mad_temp = mad(temperature),
  #          mean_temp = mean(temperature),
  #          min_temp = min(temperature),
  #          max_temp = max(temperature),
  #          diff_temp = diff(range(temperature))), by = classification, on = 'start']
  # plot(classification~distance, distances, cex = 0.5, pch = 15)
  # abline(v = x$trace_distance[heated==TRUE]$distance[1])
  # 
  # tmp <- x$trace_data[x$trace_time[type=='heating'], 
  #                     list(mean_temp = mean(temperature)), 
  #                     by = distance, on = 'start']
  # par(new = TRUE)
  # plot(mean_temp~distance, tmp, type='l', col = 'red')
  # tmp <- x$trace_data[x$trace_time[type=='ambient'], 
  #                     list(mean_temp = mean(temperature)), 
  #                     by = distance, on = 'start']
  # par(new = TRUE)
  # plot(mean_temp~distance, tmp, type='l', col = 'dark green')
  # tmp <- x$trace_data[head(x$trace_time[type=='cooling'], 2000), 
  #                     list(mean_temp = mean(temperature)), 
  #                     by = distance, on = 'start']
  # par(new = TRUE)
  # plot(mean_temp~distance, tmp, type='l', col = 'blue')
  # 
  
  
}

#' process_classification
#'
#' @param x vector of classification values
#' @param k length of median filter 
#'
#' @return
#' @export
#'
#' @examples
process_classification <- function (x, k = 31) {
  
  n <- length(x)
  
  # pad
  pad_x <- c(x[k:2], x , x[(n-1):(n-k+1)])
  
  n_new <- length(pad_x)
  
  # running median
  pad_x <- runmed(pad_x, k = k)

  # unpad
  pad_x <- pad_x[(k):(n_new-k+1)]
  
  pad_x
  
}

#' process_terminal
#'
#' @param x vector of classification values
#' @param k length of median filter 
#' @param fill_value what to fill the values with
#'
#' @return
#' @export
#'
#' @examples
process_terminal <- function (x, k = 15, fill_value = NA_real_) {
  
  n <- length(x)
  inds <- n:(n-(k-1))
  x[inds] <- fill_value
  
  x
  
  
}




#' add_classification
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
add_classification <- function(x) {
  
  d <- x$trace_distance[, list(distance, classification)]
  x$trace_data[d, classification := classification, on = 'distance']
  x
  
}

#' add_heating_cooling
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
add_heating_cooling <- function(x) {
  
  labels <- x$trace_time[, list(start, type)]
  x$trace_data[labels, type := type, on = 'start']
  x
  
}

# dts <- heating_times(dts)
# dts <- heating_distance(dts)
# dts <- heated_groups(dts)
# dts <- add_classification(dts)
# 
# dts <- calibration_bath(dts)
# 
# 
# plot(a$cluster)




