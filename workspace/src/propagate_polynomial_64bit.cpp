// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

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
Rcpp::NumericMatrix propagate_polynomial_64bit(
    Rcpp::NumericMatrix ts,
    Rcpp::NumericVector start,
    size_t nPred,
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
  arma::rowvec current(start.begin(), d, true);

  arma::vec meanState(d);
  arma::mat scaleMat = arma::eye<arma::mat>(d, d);
  arma::mat invScaleMat = arma::eye<arma::mat>(d, d);

  if (normalization == "diag" || normalization == "full") {
    meanState = arma::mean(X.rows(0, n - 1), 0).t();

    if (normalization == "diag") {
      arma::vec sds = arma::stddev(X.rows(0, n - 1), 0, 0).t();
      scaleMat.diag() = 1.0 / sds;
      invScaleMat.diag() = sds;
    } else if (normalization == "full") {
      arma::mat covMat = arma::cov(X.rows(0, n - 1));
      arma::vec eigval;
      arma::mat eigvec;
      arma::eig_sym(eigval, eigvec, covMat);
      arma::mat Dinv = arma::diagmat(1.0 / arma::sqrt(eigval));
      arma::mat D = arma::diagmat(arma::sqrt(eigval));
      scaleMat = eigvec * Dinv * eigvec.t();
      invScaleMat = eigvec * D * eigvec.t();
    }

    X = (X.each_row() - meanState.t()) * scaleMat;
    current = (current - meanState.t()) * scaleMat;
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

  arma::mat prediction(nPred, d);

  for (size_t k = 0; k < nPred; ++k) {
    std::vector<double> featuresOne(p);
    evaluate_monomials_64bit(1, d, p, current.memptr(), degrees_values.data(), featuresOne.data());
    arma::rowvec next = arma::rowvec(featuresOne.data(), p, false) * coeffs;
    prediction.row(k) = next;
    current = next;
  }

  // De-normalize prediction if needed
  if (normalization != "none") {
    prediction = prediction * invScaleMat;
    prediction.each_row() += meanState.t();
  }

  return Rcpp::wrap(prediction);
}
