#include <Rcpp.h>
#include <array>
#include <mplapack/mpreal.h>
#include <mpfr.h>
#include "deriv_L63.h"
#include "rk4_y_3d.h"

static bool init_precision = [](){
  mpfr_set_default_prec(512);
  return true;
}();

using namespace Rcpp;
using std::array;
using mpfr::mpreal;

// [[Rcpp::export]]
NumericMatrix integrate_L63_y_512bit(NumericVector start, double dt_, size_t nOut, size_t nSkip) {
  if (start.size() != 3) {
    stop("Start vector must have exactly 3 elements: x, y, z.");
  }

  mpreal dt = dt_;
  
  array<mpreal, 3> state = {
    mpreal(start[0]),
    mpreal(start[1]),
    mpreal(start[2])
  };

  auto f = [](const array<mpreal, 3>& s) {
    return lorenz63<mpreal>(s);
  };

  auto result = rk4_y_3d<mpreal>(state, dt, nOut, nSkip, f);

  NumericMatrix out(nOut, 4);
  for (size_t i = 0; i < nOut; ++i) {
    out(i, 0) = static_cast<double>(result[i][0]);
    out(i, 1) = static_cast<double>(result[i][1]);
    out(i, 2) = static_cast<double>(result[i][2]);
    out(i, 3) = static_cast<double>(result[i][3]);
  }

  return out;
}