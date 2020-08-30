#' to_matrix
#'
#' @param x data read from a dts
#' @param ... 
#'
#' @return
#' @rdname to_matrix
#' @export
#'
#' @examples
to_matrix <- function(x, ...) UseMethod("to_matrix")



#' @rdname to_matrix
#' @export
to_matrix.dts <- function(x, ...) {
  
  wh <- x$event_info$wh
  distance <- x$event_info$distance[wh]
  
  tr <- (map(x$dts, .f = function(x, ...) {
    temperature = x$trace_data[wh]
  }, wh = wh))
  
  start_times <- get_times(dts)
  
  m <- matrix(unlist(tr), nrow = length(wh))
  
  rownames(m) <- distance
  colnames(m) <- start_times
  
  m
  
}


#' @rdname to_matrix
#' @export
to_matrix.data.table <- function(x, col_name = 'temperature') {
  
  setkey(x, start, distance)
  
  distance <- unique(x[['distance']])
  start <- unique(x[['start']])
  nr <- length(distance)
  
  m  <- matrix(x[[col_name]], nrow = nr)
  
  rownames(m) <- distance
  colnames(m) <- start
  
  m
}

#' @rdname to_matrix
#' @export
to_matrix.dts_long <- function(x, col_name = 'temperature') {
  
  to_matrix(get_data_table(x), col_name)
  
}


#' #' @rdname to_matrix
#' #' @export
#' to_matrix.dts_time_subset <- function(x, col_name = 'temperature') {
#'   
#'   to_matrix(get_data_table(x), col_name)
#'   
#' }

