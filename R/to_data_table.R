#' #' to_data_table
#' #'
#' #' This function takes a DTS data set after heating times and distances are isolated and adds a column of elapsed_time and log_elapsed time. If the dataset is xt and is to be interpolated, argument 'xt' is to be set TRUE and an input for 'ultima_inpue' is specified. 
#' #' @param x the DTS data set
#' #' @param xt is set to FALSE by default. If TRUE, the data is to be interpolated and ultima_input must be specified.
#' #' @param ultima_input is only specified when xt is TRUE for interpolation. 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' to_data_table <- function(x, xt=FALSE, ultima_input=NULL) {
#'   
#'   if (xt==FALSE) {
#'     #create a data.table with distance as columns, temperature as rows
#'     heat_matrix <- data.table(t(to_matrix(x)))
#'   }
#'   if (xt==TRUE) {
#'     heat_matrix <- interpolate_to_ultima(x, ultima_input)
#'   }
#'   
#'   #create vector of elapsed time for heating
#'   elapsed_time <- x$trace_time[type=='heating']$elapsed_time
#'   log_elapsed_time <- log(elapsed_time)
#'   
#'   #bind elapsed time with heat_matrix as data.table
#'   data <- data.table(elapsed_time, log_elapsed_time, heat_matrix)
#'   
#'   #remove first column because of '-inf' row caused by log time 
#'   data <- data[-1]
#'   
#'   return(data)
#' }