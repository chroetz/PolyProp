// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// Helper function remains unchanged
void evaluate_monomials_64bit(
    size_t n, size_t d, size_t p, const double *x, const size_t *degrees, double *out_values
) {
  std::vector<std::vector<double>> powers(d);
  
  for (size_t j = 0; j < d; ++j) {
    size_t maxDegree = 0;
    for (size_t k = 0; k < p; ++k) {
      maxDegree = std::max(maxDegree, degrees[k + j * p]);
    }
    
    powers[j].resize(n * (maxDegree + 1));
    
    for (size_t i = 0; i < n; ++i) {
      powers[j][i] = 1.0;
    }
    
    for (size_t l = 1; l <= maxDegree; ++l) {
      for (size_t i = 0; i < n; ++i) {
        powers[j][i + l * n] = powers[j][i + (l - 1) * n] * x[i + j * n];
      }
    }
  }
  
  for (size_t i = 0; i < n; ++i) {
    for (size_t k = 0; k < p; ++k) {
      double v = 1.0;
      for (size_t j = 0; j < d; ++j) {
        size_t deg = degrees[k + j * p];
        if (deg == 0) continue;
        v *= powers[j][i + deg * n];
      }
      out_values[i + k * n] = v;
    }
  }
}

// [[Rcpp::export]]
Rcpp::NumericMatrix fit_polynomial_64bit(
    Rcpp::NumericMatrix ts,
    Rcpp::IntegerMatrix degrees,
    std::string normalization = "none"
) {
  size_t n = ts.nrow();
  size_t m = n - 1;
  size_t d = ts.ncol();
  size_t p = degrees.nrow();
  
  if (d != static_cast<size_t>(degrees.ncol())) {
    Rcpp::stop("dimension mismatch");
  }
  
  arma::mat X(ts.begin(), ts.nrow(), d, true);
  
  arma::vec meanState(d);
  arma::mat scaleMat = arma::eye<arma::mat>(d, d);
  
  if (normalization == "diag" || normalization == "full") {
    meanState = arma::mean(X.rows(0, n - 1), 0).t();
    
    if (normalization == "diag") {
      arma::vec sds = arma::stddev(X.rows(0, n - 1), 0, 0).t();
      scaleMat.diag() = 1.0 / sds;
    } else if (normalization == "full") {
      arma::mat covMat = arma::cov(X.rows(0, n - 1));
      arma::vec eigval;
      arma::mat eigvec;
      arma::eig_sym(eigval, eigvec, covMat);
      arma::mat Dinv = arma::diagmat(1.0 / arma::sqrt(eigval));
      scaleMat = eigvec * Dinv * eigvec.t();
    }
    
    X = (X.each_row() - meanState.t()) * scaleMat;
    
  } else if (normalization != "none") {
    Rcpp::stop("Unknown normalization type: " + normalization);
  }
  
  std::vector<double> inputs(m * d);
  std::vector<double> outputs(m * d);
  std::vector<double> features(m * p);
  std::vector<size_t> degrees_values(p * d);
  
  for (size_t k = 0; k < p; ++k) {
    for (size_t j = 0; j < d; ++j) {
      degrees_values[k + j * p] = degrees(k, j);
    }
  }
  
  for (size_t i = 0; i < m; ++i) {
    for (size_t j = 0; j < d; ++j) {
      inputs[i + j * m] = X(i, j);
      outputs[i + j * m] = X(i + 1, j);
    }
  }
  
  evaluate_monomials_64bit(m, d, p, inputs.data(), degrees_values.data(), features.data());
  
  arma::mat F(features.data(), m, p, false);
  arma::mat Y(outputs.data(), m, d, false);
  
  arma::mat A = F.t() * F;
  arma::mat B = F.t() * Y;
  
  arma::mat coeffs;
  bool status = arma::solve(
    coeffs, A, B,
    arma::solve_opts::refine +
      arma::solve_opts::equilibrate +
      arma::solve_opts::allow_ugly +
      arma::solve_opts::likely_sympd
  );
  
  if (!status) {
    Rcpp::stop("Error: failed to solve linear system");
  }
  
  return Rcpp::wrap(coeffs);
}
