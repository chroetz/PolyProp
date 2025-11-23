// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

void evaluate_monomials_32bit(
  size_t n, size_t d, size_t p, const float *x, const size_t *degrees, float *out_values
) {
  std::vector<std::vector<float>> powers(d);

  for (size_t j = 0; j < d; ++j) {
    size_t maxDegree = 0;
    for (size_t k = 0; k < p; ++k) {
      maxDegree = std::max(maxDegree, degrees[k + j * p]);
    }

    powers[j].resize(n * (maxDegree + 1));

    for (size_t i = 0; i < n; ++i) {
      powers[j][i] = 1.0f;
    }

    for (size_t l = 1; l <= maxDegree; ++l) {
      for (size_t i = 0; i < n; ++i) {
        powers[j][i + l * n] = powers[j][i + (l - 1) * n] * x[i + j * n];
      }
    }
  }

  for (size_t i = 0; i < n; ++i) {
    for (size_t k = 0; k < p; ++k) {
      float v = 1.0f;
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
Rcpp::NumericMatrix propagate_polynomial_32bit(
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

  std::vector<float> data_float(n * d);
  double* ts_ptr = ts.begin();
  for (size_t i = 0; i < n * d; ++i) {
    data_float[i] = static_cast<float>(ts_ptr[i]);
  }
  arma::fmat X(data_float.data(), n, d, false, true);

  arma::frowvec current(start.size());
  for (size_t i = 0; i < d; ++i) current(i) = static_cast<float>(start(i));

  arma::fvec meanState(d);
  arma::fmat scaleMat = arma::eye<arma::fmat>(d, d);
  arma::fmat invScaleMat = arma::eye<arma::fmat>(d, d);

  if (normalization == "diag" || normalization == "full") {
    meanState = arma::mean(X.rows(0, n - 1), 0).t();

    if (normalization == "diag") {
      arma::fvec sds = arma::stddev(X.rows(0, n - 1), 0, 0).t();
      scaleMat.diag() = 1.0f / sds;
      invScaleMat.diag() = sds;
    } else if (normalization == "full") {
      arma::fmat covMat = arma::cov(X.rows(0, n - 1));
      arma::fvec eigval;
      arma::fmat eigvec;
      arma::eig_sym(eigval, eigvec, covMat);
      arma::fmat Dinv = arma::diagmat(1.0f / arma::sqrt(eigval));;
      arma::fmat D = arma::diagmat(arma::sqrt(eigval));;
      scaleMat = eigvec * Dinv * eigvec.t();
      invScaleMat = eigvec * D * eigvec.t();
    }

    X = (X.each_row() - meanState.t()) * scaleMat;
    current = (current - meanState.t()) * scaleMat;
  } else if (normalization != "none") {
    Rcpp::stop("Unknown normalization type: " + normalization);
  }

  std::vector<float> inputs(m * d);
  std::vector<float> outputs(m * d);
  std::vector<float> features(m * p);
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

  evaluate_monomials_32bit(m, d, p, inputs.data(), degrees_values.data(), features.data());

  arma::fmat F(features.data(), m, p, false);
  arma::fmat Y(outputs.data(), m, d, false);

  arma::fmat A = F.t() * F;
  arma::fmat B = F.t() * Y;

  arma::fmat coeffs;
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

  arma::fmat prediction(nPred, d);

  for (size_t k = 0; k < nPred; ++k) {
    std::vector<float> featuresOne(p);
    evaluate_monomials_32bit(1, d, p, current.memptr(), degrees_values.data(), featuresOne.data());
    arma::frowvec next = arma::frowvec(featuresOne.data(), p, false) * coeffs;
    prediction.row(k) = next;
    current = next;
  }

  // De-normalize prediction if needed
  if (normalization != "none") {
    prediction = prediction * invScaleMat;
    prediction.each_row() += meanState.t();
  }

  // Convert back to double to return to R
  Rcpp::NumericMatrix result(nPred, d);
  for (size_t i = 0; i < nPred; ++i)
    for (size_t j = 0; j < d; ++j)
      result(i, j) = static_cast<double>(prediction(i, j));

  return result;
}
