#include <Rcpp.h>
#include <vector>
#include "rk4_3d.h"

using std::sin;
#include "deriv_TCSA.h"

using namespace Rcpp;
using std::vector;

// [[Rcpp::export]]
NumericMatrix integrate_TCSA_64bit(NumericVector start, double dt, size_t nSteps) {
  if (start.size() != 3) {
    stop("Start vector must have exactly 3 elements: x, y, z.");
  }

  vector<double> state = { start[0], start[1], start[2] };

  auto f = [](const vector<double>& s) {
    return tcsa<double>(s);
  };

  auto result = rk4_3d<double>(state, dt, nSteps, f);

  NumericMatrix out(nSteps, 4);
  for (size_t i = 0; i < nSteps; ++i)
    for (size_t j = 0; j < 4; ++j)
      out(i, j) = result[i][j];

  return out;
}
