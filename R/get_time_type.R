#' get_time_type
#'
#' @param x
#' @param time_type
#' @param start_time
#' @param end_time
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_time_type <- function(x,
                          time_type = 'heating',
                          start_time = NULL,
                          end_time = NULL, 
                          ...) UseMethod("get_time_type")

#' @rdname get_time_type
#' @export
get_time_type.data.table <- function(x, 
                                     time_type = 'heating', 
                                     start_time = NULL, 
                                     end_time = NULL, 
                                     ...) { 
  
  x[between(start, start_time, end_time)]
  
}

#' @rdname get_time_type
#' @export
get_time_type.dts_long <- function(x,
                                   time_type = 'heating',
                                   start_time = NULL,
                                   end_time = NULL,
                                   ...) { 
  
  s <- min(get_time_table(x)[type==time_type]$start)
  e <- max(get_time_table(x)[type==time_type]$start)
  
  x$trace_data <- get_data_table(x)[between(start, s, e)]
  x$trace_data <- x$trace_data[get_time_table(x), elapsed_time := elapsed_time, on = 'start']
  x
  # add elapsed time
  

  
}

