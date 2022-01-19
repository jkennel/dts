#' time_breakpoints
#'
#' @param x data read from a dts
#' @param shift distance between differences in number of samples
#' @param col_name 
#' @param log_type one of heating, cooling, or both 
#'
#' @return
#' @rdname time_breakpoints
#' @export
#'
#' @examples
time_breakpoints <- function(x, shift, col_name, type) UseMethod("time_breakpoints")


#' @rdname time_breakpoints
#' @export
time_breakpoints.default <- function(x, 
                                     shift = -10,
                                     col_name = 'temperature',
                                     type = 'both') {
  
  time_breakpoints(to_matrix(x, col_name = col_name), shift)
  
}


#' @rdname time_breakpoints
#' @export
time_breakpoints.matrix <- function(x, 
                                    shift = -5, 
                                    col_name = 'temperature',
                                    type = 'both') {

  by_time  <- colMeans(diff_by_trace(x, shift, 1))
  rn <- as.POSIXct(as.numeric(colnames(x)), origin = '1970-01-01', tz = 'UTC')
  
  if(type == 'heating') {
    return(list(on  = rn[which.min(by_time)], 
                off = max(rn) + 1.0))
  } else if (type == 'both') {
    return(list(on  = rn[which.min(by_time)], 
                off = rn[which.max(by_time)]))
  } else if (type == 'cooling') {
    return(list(on  = min(rn) - 1.0, 
                off = rn[which.max(by_time)]))
    
  }
  
}




