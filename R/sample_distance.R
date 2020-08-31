#' sample_distance
#'
#' @param x data read from a dts
#' @param n_traces
#' @param ... 
#'
#' @return
#' @rdname sample_distance
#' @export
#'
#' @examples
sample_distance <- function(x, n_traces = 100, ...) UseMethod("sample_distance")


#' @rdname sample_distance
#' @export
sample_distance.dts_long <- function(x, n_traces = 100) {
  
  d <- get_distance_table(x)
  n <- nrow(d)
  
  if(n_traces < n) {
    ind <- as.integer(seq.int(1, n, length.out = n_traces))
  } else { 
    warning('More samples than length of test')
    return(x)
  }
  
  d <- d[ind, list(distance)]
  
  x_sub <- get_data_table(x)[d, on = 'distance']

  x_sub
  
}



#' sample_times
#'
#' @param x data read from a sample_times
#' @param n_traces
#' @param ... 
#'
#' @return
#' @rdname sample_times
#' @export
#'
#' @examples
sample_times <- function(x, n_traces = 100, ...) UseMethod("sample_times")


#' @rdname sample_times
#' @export
sample_times.dts_long <- function(x, n_traces = 100) {
  
  d <- x[['trace_time']]
  n <- nrow(d)
  
  if(n_traces < n %/% 2) {
    ind <- as.integer(seq.int(1, n %/% 2, length.out = n_traces))
  } else { 
    warning('More samples than length of test')
    return(x)
  }
  
  d <- d[ind, list(start)]
  
  x_sub <- x[['trace_data']][d, on = 'start']
  
  x_sub
  
}





