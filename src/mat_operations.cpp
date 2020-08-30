// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

//' @title correlation for dts matrix by row or by column
//' 
//' 
//' @param x
//' @param dim
//' 
//' @examples
//' 
//' @export
// [[Rcpp::export]]
arma::vec cor_by_trace(arma::mat x, int dim) {
  
  int n;
  
  if (dim == 0) {
    n = x.n_cols;
  } else {
    n = x.n_rows;
  }
  
  arma::vec out(n);
  out.zeros();
  
  if (dim == 0) {
    for (int i=0; i < n-1; i++) {
      out(i) = arma::as_scalar(arma::cor(x.col(i), x.col(i+1)));
    }
  } else {
    for (int i=0; i < n-1; i++) {
      out(i) = arma::as_scalar(arma::cor(x.row(i), x.row(i+1)));
    }
  }
  
  
  return out;
  
}




//' @title Difference between dts matrix by row or by column
//' 
//' 
//' @param x matrix
//' @param shift how many values to shift
//' @param dim by row or by column
//' 
//' @examples
//' 
//' @export
// [[Rcpp::export]]
arma::mat diff_by_trace(arma::mat x, int shift, int dim) {
  
  x = x - arma::shift(x, shift, dim = dim);
  
  if(shift < 0) {
    if(dim == 0) {
      x.tail_rows(-shift).fill(0);
    } else {
      x.tail_cols(-shift).fill(0);
    }
  } else {
    if(dim == 0) {
      x.head_rows(shift).fill(0);
    } else {
      x.head_cols(shift).fill(0);
    }
  }
    
  return x;
  
}


//' @title solve_arma
//' 
//' 
//' @param x matrix
//' @param y vector
//' 
//' @examples
//' 
//' @export
// [[Rcpp::export]]
arma::mat solve_arma(const arma::mat x, const arma::mat y) {
 
 return(arma::solve(x, y));
  
}

// //' @title Sum the difference between dts matrix by row or by column
// //' 
// //' 
// //' @param x
// //' @param dim
// //' 
// //' @examples
// //' 
// //' @export
// // [[Rcpp::export]]
// arma::mat diff_first(arma::mat x, int dim) {
//   
//   x = x - arma::shift(x, -1, dim = dim);
//   
//   return arma::mean(x, dim = dim);
//   
// }
