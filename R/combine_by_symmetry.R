#' combine_by_symmetry
#'
#' @param x 
#' @return
#' 
#' @rdname combine_by_symmetry
#' @export
#'
#' @examples
combine_by_symmetry <- function(x, ...) UseMethod("combine_by_symmetry")


#' @rdname combine_by_symmetry
#' @export
combine_by_symmetry.numeric <- function(x) {
  
  n <- length(x)
  
  out <- (x[1:(n / 2L)] + (x[(n - 1):(n / 2L)])) / 2L

  out

}


#' @rdname combine_by_symmetry
#' @export
combine_by_symmetry.data.table <- function(x, col_name = 'temperature') {
  
  x[, list(temperature = combine_by_symmetry(get(col_name)), 
           distance = head(distance, .N / 2L)), by = start]
  
  
}


#' @rdname combine_by_symmetry
#' @export
combine_by_symmetry.dts_long <- function(x, col_name = 'temperature') {
  
  x$trace_data     <- combine_by_symmetry(get_data_table(x), col_name)
  x$trace_distance <- get_distance_table(x)[distance %in% unique(get_data_table(x)[['distance']])]
  x
  
}






# Find the forward trace in the back side
#' match_region
#'
#' @param x 
#' @param probs 
#' @param resolution_sub 
#'
#' @return
#' @export
#'
#' @examples
match_region <- function(x,
                         probs = c(0.05, 0.10), 
                         resolution_sub = 0.01) {
  
  
  si <- x$device$step_increment
  d  <- get_distance_table(x)
  m  <- to_matrix(x)
  
  wh <- which(d$distance > 0)
  # n  <- length(wh)
  
  # greater_than_zero <- wh[1]
  
  
  rng <- round(quantile(wh, probs = probs))
  inds <- rng[1]:rng[2]
  n_ind <- length(inds)
  
  
  d[, align_res := rolling_diff(m, inds)]
  match_ind  <- which.min(d$align_res) + 1
  
  refinement <- refine_match(m[match_ind:(match_ind - n_ind + 1), ], 
                             m[inds, ], 
                             resolution_sub = resolution_sub)
  
  sh <- (0.5 / resolution_sub - median(refinement)) * resolution_sub * si

  d[, distance_both := (match_ind - 1:nrow(m)) * si + d$distance[inds[1]]]
  mid <- (d$distance[match_ind] - d$distance[inds[1]])/2+d$distance[inds[1]]
  d[which.min(abs(distance - mid)), junction := TRUE]
  d[distance > mid, distance_both := distance_both + sh]
  d[distance <= mid, distance_both := distance]
  d[, direction := NA_character_]
  d[distance <= mid, direction := 'down']
  d[distance > mid, direction := 'up']
  
  
  # up   <- x$trace_data[d[direction == "up"], on = "distance"]
  # out_distance <- d[direction == "down"]$distance
  # 
  # x$trace_data <- up[, .(temperature = approx(x = distance_both, 
  #                                   y = temperature,
  #                                   xout = out_distance, 
  #                                   rule = 2)$y, 
  #              distance = out_distance,
  #              temperature_sd = 0), by = .(direction, start)][,.(start, distance, temperature_sd, temperature)]
  # 
  # 
  # x$trace_distance <- d[direction == "down"]
  
  x$trace_distance <- d
  
  
  return(x)
  
  # pdf()
  # for(i in 1:100) {
  #   
  # tmp <- copy(get_data_table(x))
  # tmmm <- tmp[d, on = 'distance']
  # t1 <- get_time_table(x)[i]$start
  # tmmm <- tmmm[start == t1]
  # ds <- 8
  # de <- 21
  # # plot(rev(tmmm[between(distance_both, ds, de)&direction == 'up']$temperature),
  # #      tmmm[between(distance_both, ds, de)&direction == 'down']$temperature[-1
  # #                                                                           ],
  # #      type = 'p',
  # #      pch = 20,
  # #      ylim = c(6.4, 6.7),
  # #      xlim = c(6.4, 6.7))
  # 
  # 
  # plot(temperature~distance_both,
  #      tmmm[between(distance_both, ds, de)&direction == 'up'],
  #      type = 'o',
  #      pch = 20,
  #      col= 'dark red',
  #      ylim = c(6.4, 6.8))
  # points(temperature~distance_both,
  #      tmmm[between(distance_both, ds, de) & direction == 'down'],
  #      type = 'o',
  #      pch = 20,
  #      col= 'dark blue')
  # a <- approx(x = tmmm[direction == 'up']$distance_both,
  #        y = tmmm[direction == 'up']$temperature,
  #        xout = tmmm[direction == 'down']$distance_both, rule = 1)
  # points(a$x, (a$y +  tmmm[direction == 'down']$temperature)/2, col = 'black',
  #      type = 'l', lwd = 3, ylim = c(8, 8.5), xlim = c(ds, de))
  # abline(h = 6.6, lty = 2)
  # }
  # dev.off()
  # return()
  # refine match to smaller than sample spacing
  # system.time({
  #   
  #   
  # res <- c()
  # for(j in 1:ncol(m)) {
  #   
  #   y <- m[inds, j]
  #   z <- m[match_ind:(match_ind-n_ind + 1), j]
  #   
  #   res[j] <- refine_match(z, y, resolution_sub = 0.01)  
  # }
  # 
  # sh <- median(res) * si
  # })
  # 
  # 
  # d[, distance_both := (match_ind - 1:nrow(m)) * si + d$distance[inds[1]]]
  # mid <- (d$distance[match_ind] - d$distance[inds[1]])/2+d$distance[inds[1]]
  # d[which.min(abs(distance - mid)), junction := TRUE]
  # d[distance > mid, distance_both := distance_both + sh]
  # d[distance <= mid, distance_both := distance]
  # d[, direction := NA_character_]
  # d[distance <= mid, direction := 'down']
  # d[distance > mid, direction := 'up']
  # 
  # tmp <- copy(get_data_table(x))
  # tmmm <- tmp[d, on = 'distance']
  # t1 <- get_time_table(x)[6]$start
  # tmmm <- tmmm[start == t1]
  # 
  # plot(temperature~distance_both,
  #      tmmm[between(distance_both, 80, 125)&direction == 'up'],
  #      type = 'o',
  #      pch = 20,
  #      col= 'dark red',
  #      ylim = c(8, 8.5))
  # 
  # # points(temperature~distance_both, 
  # #      tmmm[between(distance_both, 80, 125)&direction == 'down'], 
  # #      type = 'o',
  # #      pch = 20,
  # #      col= 'dark blue')
  # a <- approx(x = tmmm[direction == 'up']$distance_both,
  #        y = tmmm[direction == 'up']$temperature,
  #        xout = tmmm[direction == 'down']$distance_both, rule = 1)
  # points(a$x, (a$y +  tmmm[direction == 'down']$temperature)/2, col = 'black',
  #      type = 'l', lwd = 3, ylim = c(8, 8.5), xlim = c(80, 125))
  # # setkey(tmmm, distance_both)
  # # points(runmed(temperature, 3)~distance_both, tmmm, type = 'l')
  # # 
  # plot(temperature~distance_both, tmmm[between(distance_both, 80, 125)], type = 'l')
  # # points(temperature~distance_both, tmmm, type = 'l', col = 'red')
  # abline(v = d[junction == TRUE]$distance)
  # # 
  # # 
  # # 
  # # 
  # # d[, distance_tail := rev(distance)]
  # # 
  # # [inds[1]] - match_ind * si)]
  # # sym_distances <- d$distance[inds]
  # # 
  # # 
  # # match_ind  
  # 
  # 
  
}
  

# refine_match <- function(x, y, resolution_sub = 0.01) {
#   
#   len_match    <- length(x)
#   len_template <- length(y)
#   
#   if(len_match != len_template) {
#     stop('Lengths do not match')
#   }
#     
#   
#   x_inter <- approx(x = 1:len_match,
#                     y = x, 
#                     xout = seq(1, len_match, by = resolution_sub))$y
#   y_inter <- approx(x = 1:len_match, 
#                     y = y, 
#                     xout = seq(1, len_match, by = resolution_sub))$y
#   
#   shifts <- round(0.5/resolution_sub)
#   differences <- sapply(shift(x_inter, -shifts:shifts), function(z) sum((z-y_inter)^2, na.rm = TRUE))
#   
#   return((which.min(differences) - (shifts + 1)) * resolution_sub)
#   
# }
  


# system.time({
#   res <- c()
# for(n in 1:ncol(m)) {
#   
#   y <- m[inds, n]
#   x <- m[match_ind:(match_ind-n_ind + 1), n]
# 
#   res[n] <- refine_match(x, y, resolution_sub = 0.01)  
# }
#   sh <- median(res) * dts$device$step_increment
#   
# })
#   
#   
#   
#   for(n in 1:100) {
#     
#     match_inds <- match_ind:(match_ind - n_ind + 1)
#     
# 
#     
#     n_add <- 100
#     template_inter <- approx(x = d[inds]$distance, y = match_trace, 
#                              xout = seq(min(d[inds]$distance), max(d[inds]$distance), length.out = n_ind * n_add))$y
#     
#     match_inter <- approx(x = d[match_inds]$distance, y = template_trace, 
#                           xout = seq(round(max(d[match_inds]$distance),3), round(min(d[match_inds]$distance),3), length.out = n_ind * n_add))$y
#     to_sub <- as.matrix(setDT(shift(match_inter, -250:250)))
#     mm <- matrix(rep(template_inter, 501), ncol = 501)
#     diff <- na.omit(mm-to_sub)
#     print(which.min(colSums(diff^2)))
#   }
#   
#   
#   
#   plot(to_sub[,23], type = 'l')
#   points(mm[,23], type = 'l', col = 'red')
#   
#   plot(template_inter$y, type = 'l')
#   points(match_inter$y, type = 'l', col = 'red')
#   
#   
#   
#   aa <- ccf(template_inter$y, match_inter$y, plot = FALSE)
#   aa$lag[which.max(aa$acf)]
#   plot(aa$acf, type= 'l')
#   plot(x = 1:n_ind, m[ind,100], type = 'o')
#   points(x = 1:n_ind, m[2585:(2585-n_ind + 1), 100], type = 'o', col = 'red')
#   
# }



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


