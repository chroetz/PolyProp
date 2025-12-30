#include <Rcpp.h>
#include <array>
#include "rk4_3d.h"

using std::sin;
#include "deriv_TCSA.h"

using namespace Rcpp;
using std::array;

// [[Rcpp::export]]
NumericMatrix integrate_TCSA_64bit(NumericVector start, double dt, size_t nOut) {
  if (start.size() != 3) {
    stop("Start vector must have exactly 3 elements: x, y, z.");
  }

  array<double, 3> state = {
    static_cast<double>(start[0]),
    static_cast<double>(start[1]),
    static_cast<double>(start[2])
  };

  auto f = [](const array<double, 3>& s) {
    return tcsa<double>(s);
  };

  auto result = rk4_3d<double>(state, dt, nOut, f);

  NumericMatrix out(nOut, 4);
  for (size_t i = 0; i < nOut; ++i) {
    out(i, 0) = result[i][0];
    out(i, 1) = result[i][1];
    out(i, 2) = result[i][2];
    out(i, 3) = result[i][3];
  }

  return out;
}
