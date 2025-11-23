// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <mpblas_mpfr.h>
#include <mplapack_mpfr.h>

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
Rcpp::NumericMatrix propagate_polynomial_512bit(
    Rcpp::NumericMatrix ts,
    Rcpp::NumericVector start,
    mplapackint nPred,
    Rcpp::IntegerMatrix degrees,
    std::string normalization = "none"
) {

    mplapackint n = ts.nrow();
    mplapackint m = n - 1;
    mplapackint d = ts.ncol();
    mplapackint p = degrees.nrow();
    mplapackint info = -1;

    if (normalization != "none") {
        Rcpp::stop("Normalization not implemented for 512-bit");
    }

    if (d != degrees.ncol()) {
        Rcpp::stop("dimension mismatch");
    }

    mpreal *inputs = new mpreal[m * d];
    mpreal *outputs = new mpreal[m * d];
    mpreal *features = new mpreal[m * p];

    mplapackint *degrees_values = new mplapackint[p * d];

    for (mplapackint k = 0; k < p; ++k) {
        for (mplapackint j = 0; j < d; ++j) {
            degrees_values[k + j * p] = degrees(k, j);
        }
    }

    for (mplapackint i = 0; i < m; i++) {
        for (mplapackint j = 0; j < d; j++) {
          inputs[i + j * m] = ts(i, j);
          outputs[i + j * m] = ts(i+1, j);
        }
    }

    evaluate_monomials_512bit(m, d, p, inputs, degrees_values, features);

    mpreal *a = new mpreal[p * p];
    mpreal *b = new mpreal[p * d];

    Rgemm("T", "N", p, p, m, 1, features, m, features, m, 0, a, p);
    Rgemm("T", "N", p, d, m, 1, features, m, outputs, m, 0, b, p);

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
    for (mplapackint i = 0; i < d; i++) {
      current[i] = start(i);
    }

    for (mplapackint k = 0; k < nPred; k++) {
      evaluate_monomials_512bit(1, d, p, current, degrees_values, featuresOne);
      Rgemv("T", p, d, 1, b, p, featuresOne, 1, 0, current, 1);
      for (mplapackint i = 0; i < d; i++) {
        prediction[k + i * nPred] = current[i];
      }
    }

    // Copy the solution back to Rcpp::NumericMatrix
    Rcpp::NumericMatrix result(nPred, d);
    for (mplapackint k = 0; k < nPred; k++) {
        for (mplapackint i = 0; i < d; i++) {
            result(k, i) = prediction[k + i * nPred];
        }
    }

    // Clean up
    delete[] inputs;
    delete[] outputs;
    delete[] a;
    delete[] b;
    delete[] ipiv;
    delete[] prediction;
    delete[] features;
    delete[] featuresOne;
    delete[] current;
    delete[] degrees_values;

    return result;
}
