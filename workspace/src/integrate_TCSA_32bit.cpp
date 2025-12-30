#include <Rcpp.h>
#include <array>
#include "rk4_3d.h"

using std::sin;
#include "deriv_TCSA.h"

using namespace Rcpp;
using std::array;

// [[Rcpp::export]]
NumericMatrix integrate_TCSA_32bit(NumericVector start, float dt, size_t nOut) {
  if (start.size() != 3) {
    stop("Start vector must have exactly 3 elements: x, y, z.");
  }

  array<float, 3> state = {
    static_cast<float>(start[0]),
    static_cast<float>(start[1]),
    static_cast<float>(start[2])
  };

  auto f = [](const array<float, 3>& s) {
    return tcsa<float>(s);
  };

  auto result = rk4_3d<float>(state, dt, nOut, f);

  NumericMatrix out(nOut, 4);
  for (size_t i = 0; i < nOut; ++i) {
    out(i, 0) = result[i][0];
    out(i, 1) = result[i][1];
    out(i, 2) = result[i][2];
    out(i, 3) = result[i][3];
  }

  return out;
}
