#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix convert_to_32bit(Rcpp::NumericMatrix x)
{
  size_t n = x.nrow();
  size_t d = x.ncol();

  std::vector<float> data_float(n * d);
  double* x_ptr = x.begin();
  for (size_t i = 0; i < n * d; ++i) {
    data_float[i] = static_cast<float>(x_ptr[i]);
  }

  Rcpp::NumericMatrix result(n, d);
  for (size_t i = 0; i < n; ++i)
    for (size_t j = 0; j < d; ++j)
      result(i, j) = static_cast<double>(data_float[i + j*n]);

  return result;
}
