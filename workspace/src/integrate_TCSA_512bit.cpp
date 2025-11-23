#include <Rcpp.h>
#include <vector>
#include <mplapack/mpreal.h>
#include <mpfr.h>
#include "rk4_3d.h"

#include "deriv_TCSA.h"

static bool init_precision = [](){
  mpfr_set_default_prec(512);
  return true;
}();

using namespace Rcpp;
using std::vector;
using mpfr::mpreal;

// [[Rcpp::export]]
NumericMatrix integrate_TCSA_512bit(NumericVector start, double dt_, size_t nSteps) {
  if (start.size() != 3) {
    stop("Start vector must have exactly 3 elements: x, y, z.");
  }

  mpreal dt = dt_;
  vector<mpreal> state = {
    mpreal(start[0]),
    mpreal(start[1]),
    mpreal(start[2])
  };

  auto f = [](const vector<mpreal>& s) {
    return tcsa<mpreal>(s);
  };

  auto result = rk4_3d<mpreal>(state, dt, nSteps, f);

  NumericMatrix out(nSteps, 4);
  for (size_t i = 0; i < nSteps; ++i)
    for (size_t j = 0; j < 4; ++j)
      out(i, j) = static_cast<double>(result[i][j]);

  return out;
}
