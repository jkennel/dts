#' combine_by_symmetry
#'
#' @param x data read from a dts
#'
#' @return
#' @rdname combine_by_symmetry
#' @export
#'
#' @examples
combine_by_symmetry <- function(x, ...) UseMethod("combine_by_symmetry")


#' @rdname combine_by_symmetry
#' @export
combine_by_symmetry.numeric <- function(x) {
  
  n <- length(x)
  
  tmp <- (x[1:(n / 2)] +
            (x[(n - 1):(n / 2)])) / 2
  
}

#' @rdname combine_by_symmetry
#' @export
combine_by_symmetry.data.table <- function(x, col_name = 'temperature') {
  
  x[, list(temperature = combine_by_symmetry(get(col_name)), 
           distance = head(distance, .N/2)), by = start]
  
}


#' @rdname combine_by_symmetry
#' @export
combine_by_symmetry.dts_long <- function(x, col_name = 'temperature') {
  
  x$trace_data <- combine_by_symmetry(x$trace_data, col_name)
  
  x
}





#' half_data
#'
#' @param x data read from a dts
#' @param x type read from a dts
#'
#' @return
#' @rdname half_data
#' @export
#'
#' @examples
half_data <- function(x, type = 'head', ...) UseMethod("half_data")



#' @rdname half_data
#' @export
half_data.dts_long <- function(x, type = 'head') {
  
  td <- get_distance_table(x)
  
  n  <- nrow(td)
  n_div_2 <- n %/% 2
  
  if(type == 'tail') {
    d <- td[!seq_len(n_div_2)]
  } else {
    d <- td[seq_len(n_div_2)]
  }
  
  x[['trace_distance']] <- d
  x[['trace_data']] <- get_data_table(x)[d[, list(distance)], on = 'distance']
  
  x
  
}


