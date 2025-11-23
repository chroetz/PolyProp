#include <Rcpp.h>
#include <vector>
#include "deriv_L63.h"
#include "rk4_3d.h"

using namespace Rcpp;
using std::vector;

// [[Rcpp::export]]
NumericMatrix integrate_L63_32bit(NumericVector start, float dt, size_t nSteps) {
  if (start.size() != 3) {
    stop("Start vector must have exactly 3 elements: x, y, z.");
  }

  vector<float> state = {
    static_cast<float>(start[0]),
    static_cast<float>(start[1]),
    static_cast<float>(start[2])
  };

  auto f = [](const vector<float>& s) {
    return lorenz63<float>(s);
  };

  auto result = rk4_3d<float>(state, dt, nSteps, f);

  NumericMatrix out(nSteps, 4);
  for (size_t i = 0; i < nSteps; ++i)
    for (size_t j = 0; j < 4; ++j)
      out(i, j) = result[i][j];

  return out;
}
