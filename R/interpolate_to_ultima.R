#' interpolate_to_ultima
#'
#' @param x the XT DTS data as a list of data tables
#' @param ultima_input is the directory of ultima data to interpolate to. Must be .RDS file
#'
#' @return
#' @export
#'
#' @examples


interpolate_to_ultima <- function(x, ultima_input) 
{
  dts_ultima <- readRDS(ultima_input)
  
  # the values that will be interpolated to
  x_out <- dts_ultima$trace_distance$distance
  
  # formatting the heat matrix for interpolation
  working <- to_matrix(x)
  dpth <- as.numeric(rownames(working))
  working <- data.table(working)
  
  # interpolate
  interpolated <- data.table(x_out)
  for (i in 1:ncol(working)) {
    x <- dpth
    y <- working[[i]]
    new <- approx(x, y, xout=x_out)
    interpolated <- cbind(interpolated, new$y)
  }
  interpolated <- na.omit(interpolated)
  
  # save to name columns
  cols <- interpolated$x_out
  
  #remove x_out
  df <- subset(interpolated, select = -x_out)
  df <- data.table(t(as.matrix(df)))
  colnames(df) <- as.character(cols)
  
  # now reassign column names to interpolated
  return(df) 
}
