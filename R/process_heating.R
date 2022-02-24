#' process_heating
#'
#' This function takes a DTS dataset and performs rudimentary processing steps to calibrate and find heating times and distances. 
#' @param x the DTS dataset
#' @param n the spacing between samples in time to calculate the difference.
#'  Should be a negative number.
#' @param heating_type the type of test.  Can be "heating" or "both".
#'
#' @return
#' @export
#'
#' @examples
process_heating <- function(dts, set_back=0, heating='heating', subset_by='heated') {
  #get rid of zero values if present in dts
  if (to_matrix(dts)[1,1] == '0') {
    
    dist <- dts$trace_distance
    test1 <- dist[with(dist, distance > 0),]
    dts$trace_distance <- test1
    
    dist <- dts$trace_data
    test2 <- dist[with(dist, distance > 0),]
    dts$trace_data <- test2
  }
  
  # step find water bath
  dts <- find_water_bath(dts, buffer = 0.05)
  
  # step shift to
  dts <- bath_calibration(dts, smooth = TRUE)
  
  # step find heating times
  dts <- heating_time(dts, heating_type = heating)
  
  # find index where heating starts
  num <- dts$trace_time[type==heating, which=TRUE]
  
  # split time data
  g <- dts$trace_time[1:(num[1]-(set_back+1)),]
  p <- dts$trace_time[(num[1]-set_back):nrow(dts$trace_time),]
  p$type = "heating"
  # create new time sequence so 0 starts where set_back specified
  z <- seq(0, length.out=(nrow(p)), by=180)
  p$elapsed_time <- z
  dts$trace_time <- rbind(g, p)
  
  # find heating distances
  dts <- heating_distance(dts, heating_type = heating) 
  
  # remove distances in unit
  dts <- subset_distance(dts, by = subset_by)  
  
  #isolate only heating data
  heat <- get_time_type(dts, time_type = heating)
  
  #make vectors for elapsed time and log elapsed time
  heat$trace_time[type =='heating', log_elapsed_time := log(elapsed_time)]
  
  return(heat)
}
