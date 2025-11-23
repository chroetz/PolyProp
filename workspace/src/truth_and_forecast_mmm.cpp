// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <mpblas_mpfr.h>
#include <mplapack_mpfr.h>
#include "deriv_L63.h"

using namespace Rcpp;
using mpfr::mpreal;

// Set default precision to 512 bits via the MPFR C API.
static bool init_precision = [](){
  mpfr_set_default_prec(512);
  return true;
}();


void evaluate_monomials_512bit(
    mplapackint n, mplapackint d, mplapackint p, mpreal *x, mplapackint *degrees, mpreal *out_values
) {

    mpreal **powers = new mpreal*[d];

    for (mplapackint j = 0; j < d; ++j) {
        mplapackint maxDegree = 0;
        for (mplapackint k = 0; k < p; ++k) {
            if (degrees[k + j * p] > maxDegree) {
                maxDegree = degrees[k + j * p];
            }
        }

        powers[j] = new mpreal[n * (maxDegree + 1)];

        for (mplapackint i = 0; i < n; ++i) {
            powers[j][i] = 1;
        }

        for (mplapackint l = 1; l <= maxDegree; ++l) {
            for (mplapackint i = 0; i < n; ++i) {
                powers[j][i + l * n] = powers[j][i + (l - 1) * n] * x[i + j * n];
            }
        }
    }

    for (mplapackint i = 0; i < n; ++i) {
        for (mplapackint k = 0; k < p; ++k) {
            mpreal v = 1;
            for (mplapackint j = 0; j < d; ++j) {
                if (degrees[k + j * p] == 0) continue;
                v *= powers[j][i + degrees[k + j * p] * n];
            }
            out_values[i + k * n] = v;
        }
    }

    for (mplapackint j = 0; j < d; ++j) {
        delete[] powers[j];
    }
    delete[] powers;
}



// [[Rcpp::export]]
Rcpp::NumericMatrix integrate_L63_and_propagate_polynomial_512bit(
    NumericVector start,
    double solverDt,
    mplapackint nStep,
    mplapackint nObs,
    mplapackint nPred,
    Rcpp::IntegerMatrix degrees
) {

  mplapackint d = 3;
  mplapackint n = nObs - 1;
  mpreal dt = solverDt * nStep;
  mpreal dt_small = solverDt;
  std::vector<mpreal> state = { start[0], start[1], start[2] };
  mplapackint m = nObs+nPred;

  mpreal *ts = new mpreal[m * d];

  std::vector<mpreal> temp1(3);
  std::vector<mpreal> temp2(3);
  std::vector<mpreal> temp3(3);

  auto f = [](const std::vector<mpreal>& s) {
    return lorenz63<mpreal>(s);
  };
  
  for (mplapackint i = 0; i < m; ++i) {

    // Store current state in the output matrix.
    ts[i + 0 * m] = state[0];
    ts[i + 1 * m] = state[1];
    ts[i + 2 * m] = state[2];

    for (mplapackint k = 0; k < nStep; ++k) { // RK4 at nStep times higher accuracy
      // Compute RK4 stages.
      auto k1 = f(state);

      for (mplapackint j = 0; j < 3; ++j)
        temp1[j] = state[j] + 0.5 * dt_small * k1[j];
      auto k2 = f(temp1);

      for (mplapackint j = 0; j < 3; ++j)
        temp2[j] = state[j] + 0.5 * dt_small * k2[j];
      auto k3 = f(temp2);

      for (mplapackint j = 0; j < 3; ++j)
        temp3[j] = state[j] + dt_small * k3[j];
      auto k4 = f(temp3);

      // Update state using RK4 formula.
      for (mplapackint j = 0; j < 3; ++j) {
        state[j] += dt_small / 6.0 * (k1[j] + 2 * k2[j] + 2 * k3[j] + k4[j]);
      }
    }
  }


  mplapackint p = degrees.nrow();
  mplapackint info = -1;

  if (d != degrees.ncol()) {
    Rcpp::stop("dimension mismatch");
  }

  mpreal *inputs = new mpreal[n * d];
  mpreal *outputs = new mpreal[n * d];
  mpreal *features = new mpreal[n * p];

  mplapackint *degrees_values = new mplapackint[p * d];

  for (mplapackint k = 0; k < p; ++k) {
    for (mplapackint j = 0; j < d; ++j) {
        degrees_values[k + j * p] = degrees(k, j);
    }
  }

  for (mplapackint i = 0; i < n; i++) {
    for (mplapackint j = 0; j < d; j++) {
      inputs[i + j * n] = ts[i + j * m];
      outputs[i + j * n] = ts[i+1 + j * m];
    }
  }

  evaluate_monomials_512bit(n, d, p, inputs, degrees_values, features);

  mpreal *a = new mpreal[p * p];
  mpreal *b = new mpreal[p * d];

  Rgemm("T", "N", p, p, n, 1, features, n, features, n, 0, a, p);
  Rgemm("T", "N", p, d, n, 1, features, n, outputs, n, 0, b, p);

  mplapackint *ipiv = new mplapackint[p];

  Rgesv(p, d, a, p, ipiv, b, p, info);

  // Check for successful computation
  if (info != 0) {
    delete[] a;
    delete[] b;
    delete[] ipiv;
    Rcpp::stop("Error: Rgesv failed to compute the solution");
  }

  mpreal *prediction = new mpreal[nPred * d];

  mpreal *featuresOne = new mpreal[p];
  mpreal *current = new mpreal[d];
  for (mplapackint j = 0; j < d; j++) { // init current state with last observation
    current[j] = ts[nObs-1 + j*m];
  }
  for (mplapackint i = 0; i < nPred; i++) {
    evaluate_monomials_512bit(1, d, p, current, degrees_values, featuresOne);
    Rgemv("T", p, d, 1, b, p, featuresOne, 1, 0, current, 1);
    for (mplapackint j = 0; j < d; j++) {
      prediction[i + j * nPred] = current[j];
    }
  }

  Rcpp::NumericMatrix err(nPred, 2);
  mpreal t = 0;
  mpreal x = 0;
  for (mplapackint i = 0; i < nPred; i++) {
    x = 0;
    for (mplapackint j = 0; j < d; j++) {
        x += (prediction[i + j * nPred] - ts[nObs+i + j * m])*(prediction[i + j * nPred] - ts[nObs+i + j * m]);
    }
    t += dt;
    err(i, 1) = sqrt(x);
    err(i, 0) = t;
  }

  // Clean up
  delete[] inputs;
  delete[] outputs;
  delete[] a;
  delete[] b;
  delete[] ts;
  delete[] ipiv;
  delete[] prediction;
  delete[] features;
  delete[] featuresOne;
  delete[] current;
  delete[] degrees_values;

  return err;
}
