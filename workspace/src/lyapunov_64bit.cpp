#include <Rcpp.h>

using std::sin;
using std::cos;
#include "lyapunov.h"

using namespace Rcpp;


// [[Rcpp::export]]
double estimate_lyapunov_L63_64bit(double dt, size_t steps, size_t warmup, double perturbation_scale,
                             NumericVector state_in, NumericVector direction_in) {
    if (state_in.size() != 3 || direction_in.size() != 3) {
        stop("Input vectors must each have 3 elements.");
    }

    std::vector<double> state(3), direction(3);
    for (size_t i = 0; i < 3; ++i) {
        state[i] = state_in[i];
        direction[i] = direction_in[i];
    }

    return estimate_lyapunov<double>(
        &lorenz63_variational<double>,
        dt,
        steps,
        warmup,
        perturbation_scale,
        state,
        direction
    );
}

// [[Rcpp::export]]
double estimate_lyapunov_TCSA_64bit(double dt, size_t steps, size_t warmup, double perturbation_scale,
                              NumericVector state_in, NumericVector direction_in) {
    if (state_in.size() != 3 || direction_in.size() != 3) {
        stop("Input vectors must each have 3 elements.");
    }

    std::vector<double> state(3), direction(3);
    for (size_t i = 0; i < 3; ++i) {
        state[i] = state_in[i];
        direction[i] = direction_in[i];
    }

    return estimate_lyapunov<double>(
        &tcsa_variational<double>,
        dt,
        steps,
        warmup,
        perturbation_scale,
        state,
        direction
    );
}



// [[Rcpp::export]]
double estimate_lyapunov_L96_64bit(double dt, size_t steps, size_t warmup, double perturbation_scale,
                             NumericVector state_in, NumericVector direction_in) {
    if (state_in.size() != direction_in.size()) {
        stop("Input vectors must have same number of elements.");
    }

    size_t d = state_in.length();

    std::vector<double> state(d), direction(d);
    for (size_t i = 0; i < d; ++i) {
        state[i] = state_in[i];
        direction[i] = direction_in[i];
    }

    return estimate_lyapunov<double>(
        &lorenz96_variational<double>,
        dt,
        steps,
        warmup,
        perturbation_scale,
        state,
        direction
    );
}
