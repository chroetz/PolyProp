#include <Rcpp.h>
#include <vector>
#include "deriv_L96.h"
#include "rk4_nd.h"

using namespace Rcpp;
using std::vector;

// [[Rcpp::export]]
NumericMatrix integrate_L96_64bit(NumericVector start, double dt, size_t nSteps) {
  size_t d = start.length();

  vector<double> state(d);
  for (size_t j = 0; j < d; ++j)
    state[j] = start(j);

  auto f = [](const vector<double>& s) {
    return lorenz96<double>(s);
  };

  auto result = rk4_nd<double>(state, dt, nSteps, f);

  NumericMatrix out(nSteps, d+1);
  for (size_t i = 0; i < nSteps; ++i)
    for (size_t j = 0; j < d+1; ++j)
      out(i, j) = result[i][j];

  return out;
}
