#' denoise_by_trace
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
denoise_by_trace <- function(x, filter_length = 9) {
  
  mat <- to_matrix(x)
  
  # high pass filtering with removal of big changes
  mat_high_pass <- t(apply(mat, 1, function(y) {

    y <- y - aquifer::fftw_convolve(y, waterlevel::window_nuttall(filter_length))
    
    y[y > 0.2] <- NA
    y[y < -0.2] <- NA
    
    y
  }))
  

  mat_noise <- mat
  mat_noise[] <- 0
  rng <- filter_length %/% 2

  # model noise
  n <- nrow(mat_high_pass)
  n_terms <- 10
  
  for (i in (n_terms+1):(n-(n_terms+1))) {
    
    inds <- sort(unique(c(i + 1:n_terms, i - 1:n_terms)))
    
    mat_noise[i,] <- nafill(predict(
      lm(c(NA, diff(mat[i,]))~t(mat_high_pass[inds,])-1, na.action = na.exclude)), fill = 0)
    
  }
  
  x$trace_data[, temperature := as.numeric(mat-mat_noise)]
  x
  
}


#' denoise_by_trace_2
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
denoise_by_trace_2 <- function(x) {
  
  mat <- to_matrix(x)
  rng <- 1:100
  half <- mat
  for(i in 2:(nrow(mat)-1)) {
    
    a  <- diff(mat[i,])
    b  <- diff(mat[nrow(mat) - (i-2),])
    median(a)
    median(b)
    aa <- ifelse(sign(a) == sign(b), (a + b)/2, (a) / 2) 
    

    plot(a[rng], type='l')
    points(b[rng], type = 'l', col = 'red')
    points(aa[rng], col = 'green', type='l')
    points(a[rng]-aa[rng], col = 'blue', type='l')
      
    
    a2n <- diff(mat[i-2,])
    a1n <- diff(mat[i-1,])
    a1 <- diff(mat[i+1,])
    a2 <- diff(mat[i+2,])
    
    mean(a2n)
    mean(a1n)
    mean(a)
    mean(a1)
    mean(a2)
    
    sd(a2n)
    sd(a1n)
    sd(a)
    sd(a1)
    sd(a2)
    
    
    
    plot(a[rng], type='l')
    points(a1[rng], type='l', col = 'blue')
    points(a2[rng], type='l', col = 'red')
    
    
    half[i, ] <- c(0,a) - c(0,predict(lm(a~a1+a2)))
  }
  
  x$trace_data[, temperature := as.numeric(half)]
  x
  
  p <- plot_ly(z = half[,rng],

               colors = viridis(100),
               type = "heatmap") %>%
    layout(yaxis = list(autorange="reversed"))
    # colorbar(limits = c(7.5, 8.))
  p

i <- 300
  a <- diff(mat[i,])
  a1 <- diff(mat[i-1,])
  a2 <- diff(mat[i+1,])
  
  plot(a[rng], type = 'l')
  grid()
  points(a1[rng], type = 'l', col = '#00009090')
  points(a2[rng], type = 'l', col = '#00009090')
  # points(a2, type = 'l', col = '#90009090')
  # points(a3, type = 'l', col = '#90009090')
  points(a-(a1+a2)*0.5, type = 'l',lwd = 2, col = '#009000')

  # plot()
  # diff(range(mat[n,rng]))
  # diff(range(mat[831-(n-1),rng]))
  # diff(range(mat[n,rng]-mat[831-(n-1),rng]))
  # 
  # sign(diff(mat[n,rng]))
  # sign(diff(mat[831-(n-1),rng]))
  # 
  # plot(mat[n,rng], type='l',lwd = 2)
  # points(mat[831-(n-1),rng], type='l', col = 'red',lwd = 2)
  # points((mat[n,rng] + mat[831-(n-1),rng])/2, type='l', col = 'blue',lwd = 2)
  # 
  # 
  # 
  # tmpa <- (mat[n,rng] - mat[n+1,rng])
  # tmpb <- (mat[n,rng] - mat[n-1,rng])
  # 
  # 
  # tmpn3 <- (mat[n,rng] - (mat[831-(n-2),rng] + mat[831-(n-1),rng])/2)
  # tmpn2 <- (mat[n,rng] - mat[831-(n-2),rng])
  # tmpn1 <- (mat[n,rng] - mat[831-(n-1),rng])
  # tmpn <- (mat[n,rng] - mat[831-(n),rng])
  # tmp1 <- (mat[n,rng] - mat[831-(n+1),rng])
  # tmp2 <- (mat[n,rng] - mat[831-(n+2),rng])
  # tmp3 <- (mat[n,rng] - mat[831-(n+3),rng])
  # dt <- data.table(diffs = c(tmpa, tmpb, tmpn3, tmpn2, tmpn1,
  #                            tmpn,
  #                            tmp1, tmp2, tmp3), 
  #                  type = rep(c('+', '-', n + -3:3), each = length(tmpn)))
  # boxplot(diffs~type, dt)
  # grid()
  # 
  # plot(caTools::runmean((caTools::runmax(mat[50,] , 13) + 
  #        caTools::runmax(mat[831-48,], 21)) / 2, 60), type='l', col = 'red')
  # points(caTools::runmean((caTools::runmin(mat[50,] , 13) + 
  #         caTools::runmin(mat[831-48,], 21)) / 2, 60), type='l', col = 'blue')
  # points(x$trace_time$calib_temperature-0.05, col = 'green', type='l')
  # 
  # 
  # mad(abs(mat[50,] - mat[831-48,]))
  # 
  
  # high pass filtering with removal of big changes
  # mat_high_pass <- t(apply(mat, 1, function(y) {
  #   runmed(y, filter_length)
  # }))
  # 
  # x$trace_data[, temperature := as.numeric(mat_high_pass)]
  # x
}









loss_profile <- function(x) {
  c(-0.268, -0.5 * diff(log(x$stokes/x$anti_stokes) - log(x$rev_stokes/x$rev_anti_stokes)))
}

# 
# 
# alpha_dz <- loss_profile(x1)
# 
# 0 = (alpha_dz/log(c_pos / c_neg) + log(stokes/anti_stokes) / log(c_pos / c_neg))



# cut_time <- range(dts$trace_time$start[300:170])
# x1 <- x[between(start,cut_time[1], cut_time[2]), lapply(.SD, mean), by = list(distance)]
# 
# 
# mn_r <- mean((x1$rev_anti_stokes[400:430]))
# mn <- mean((x1$anti_stokes[400:430]))
# d <- mn-mn_r
# plot((x1$anti_stokes[400:430])-mn, type = 'o', pch = 20, main = 'anti-stokes', ylim = c(-150,550))
# points((((x1$rev_anti_stokes)[400:430])-mn_r), type = 'o', pch = 20, col = 'red')
# abline(v = 13)
# abline(v = 21)
# 
# mn_r <- mean((x1$rev_stokes[400:430]))
# mn <- mean((x1$stokes[400:430]))
# 
# plot((x1$stokes[400:430]-mn), type = 'o', pch = 20, main = 'stokes', ylim = c(-60,150))
# points((x1$rev_stokes[400:430]-mn_r), type = 'o', pch = 20, col = 'red')
# abline(v = 13)
# abline(v = 21)
# 
# 
# plot((x1$rev_stokes[400:430]), main = 'stokes', type = 'o', pch = 20)
# par(new = TRUE)
# plot((x1$rev_anti_stokes[400:430]), type = 'o', pch = 20, col = 'red')
# 
#           
# 
# alpha_dz <- loss_profile(x1)
# plot(alpha_dz)
# x <- dts$trace_data
# x1 <- x[as.character(start) == '2014-01-01 01:01:14']
# 
# x[, alpha_dz := cumsum(loss_profile(.SD)), by = start]
# x1[, alpha_dz := cumsum(loss_profile(.SD)), by = start]
# 
# summary(fit_alpha_1 <- lm(alpha_dz~distance, x1))
# 
# d <- (-0.190857) *(1 +(x1$alpha_dz) +log((x1$stokes)/(x1$anti_stokes)))
# # d <- (-0.190857) *(1 +(x1$distance * -5.679e-5 + 2.640e-3) +log((x1$stokes)/(x1$anti_stokes)))
# d2 <- -0.190857 *(1 +rev(x1$alpha_dz) +log(x1$rev_stokes/x1$rev_anti_stokes))
# summary(lm(x1$temperature~d))
# summary(lm(x1$temperature~d2))
# 
# plot(d~x1$distance, type='l')
# points(d2~x1$distance, type='l', col = 'red')
# mean(d-d2)
# adj <- ifelse(sign(diff(d)) == sign(diff(d2)),0,diff(d)-(diff(d) + diff(d2))/2)
# points(d+adj~x1$distance, col = 'blue', type='l')
# 
# plot((d+adj)[300:500]~x1$distance[300:500], col = 'blue', type='l')
# points(d[300:500]~x1$distance[300:500], col = 'black', type='l')

