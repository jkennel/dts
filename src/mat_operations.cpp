// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

//' @title correlation for dts matrix by row or by column
//' 
//' 
//' @param x
//' @param dim if 0 then it is by column
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

//' @title correlation for dts matrix by row or by column
//' 
//' 
//' @param x
//' @param y
//' @param dim
//' 
//' @examples
//' 
//' @export
// [[Rcpp::export]]
arma::vec cor_with_trace(arma::mat x, arma::vec y, int dim) {
  
  int n;
  
  if (dim == 0) {
    n = x.n_cols;
  } else {
    n = x.n_rows;
  }
  
  arma::vec out(n);
  out.zeros();
  
  if (dim == 0) {
    for (int i=0; i < n; i++) {
      out(i) = arma::as_scalar(arma::cor(x.col(i), y));
    }
  } else {
    for (int i=0; i < n; i++) {
      out(i) = arma::as_scalar(arma::cor(x.row(i), y));
    }
  }
  
  
  return out;
  
}



//' @title prediction for dts matrix by row or by column
//' 
//' 
//' @param x
//' @param y
//' @param dim
//' 
//' @examples
//' 
//' @export
// [[Rcpp::export]]
arma::vec residual_variance_with_trace(arma::mat x, arma::vec y, int dim) {
  
  // dim == 0 is by column
  // dim != 0 is by row
  
  int n, n_res;
  double coef;
  if (dim == 0) {
    n = x.n_cols;
    n_res = x.n_rows;
  } else {
    n = x.n_rows;
    n_res = x.n_cols;
  }
  
  arma::vec out(n);
  arma::vec res(n_res);
  out.zeros();
  res.zeros();
  
  if (dim == 0) {
    for (int i=0; i < n; i++) {
      coef = arma::as_scalar(arma::solve(x.col(i), y, arma::solve_opts::fast));    // fit model y ~ X
      out(i) = arma::var(y - x.col(i) * coef);
    }
  } else {
    for (int i=0; i < n; i++) {
      res  = x.row(i).t();
      coef = arma::as_scalar(arma::solve(res, y,  arma::solve_opts::fast));    // fit model y ~ X
      out(i) = arma::var(y - res * coef);
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



//' @title rolling_diff
//' 
//' 
//' @param x matrix
//' @param y integer vector
//' 
//' @examples
//' 
//' @export
// [[Rcpp::export]]
arma::vec rolling_diff(const arma::mat x, const arma::uvec inds) {
  
  std::size_t nr = x.n_rows;
  std::size_t max_ind = arma::max(inds);
  std::size_t ni = inds.n_elem;
  std::size_t to_ind;
  
  
  arma::mat m = x.rows(inds);
  arma::mat z = arma::flipud(x);

  arma::vec s(nr);
  s.fill(NA_REAL);
  
  for (std::size_t i = 0; i < (nr-max_ind); i++) {
    to_ind = i + ni - 1;
    
    s[nr - i - 1] = arma::accu(arma::pow((z.rows(i, i + ni - 1) - m), 2));
  
  }
  
  return(s);
}


//' @title refine_match
//' 
//' 
//' @param x matrix
//' @param y vector
//' 
//' @examples
//' 
//' @export
// [[Rcpp::export]]
arma::ivec refine_match(const arma::mat x, const arma::mat y, double resolution_sub = 0.01) {
  
  unsigned n = y.n_rows;
  unsigned nc = y.n_cols;
  
  arma::vec x_in = arma::regspace(1.0, n);
  arma::vec x_out = arma::regspace(1.0, n);
  arma::vec interps = arma::regspace(-0.5, resolution_sub, 0.5);
  unsigned n_interps = interps.n_elem;
  arma::ivec ind(nc);
  arma::vec z(n);
  arma::vec trim_x(n-2);
  arma::vec yi_up(n_interps);

  double s_new, s = 1e12;
  
  for (unsigned i = 0; i < nc; i++) {
    trim_x = x.col(i).subvec(1, n-2);
    ind[i] = 0;
    s = 1e12;
    for (unsigned j = 0; j < n_interps; j++) {
      x_in = x_out + interps[j];
      arma::interp1(x_in, y.col(i), x_out, yi_up, "*linear");
    
      s_new = arma::accu(arma::pow(trim_x - yi_up.subvec(1, n-2), 2));
      
      if (s_new < s) {
        ind[i] = j + 1;
        s = s_new;
      }
      
      
    }
    
  }  
  
  
  return(ind);
}





/*** R
library(microbenchmark)
library(collapse)

# system.time({
# bb <- rolling_diff(m, inds)
#   # cc <- refine_match(bb, m[inds,])
# })


# n <- 1e7
# x <- rnorm(n)
# microbenchmark(
#   mean_cpp(x),
#   fmean(x),
#   mean(x),
#   mean(x, na.rm = TRUE),
#   sum(x),
#   sum_cpp(x),
#   times = 10
# )

*/

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
