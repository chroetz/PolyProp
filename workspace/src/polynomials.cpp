#include <Rcpp.h>
using namespace Rcpp;

#include <vector>

//' Evaluate Monomials.
//'
//' @param x A n x d matrix. The n input vectors, each of dimension d.
//' @param degrees A p x d matrix. The degrees of the p output monomial for each of the d dimensions.
//' @returns A n x p matrix. The values of the p monomials for each of the n inputs vectors.
//' @export
// [[Rcpp::export]]
NumericMatrix evaluateMonomials(NumericMatrix x, IntegerMatrix degrees) {

  size_t n = x.nrow();
  size_t d = x.ncol();
  size_t p = degrees.nrow();
  NumericMatrix out(n, p);
  double v;

  List powers(d);
  for (size_t j = 0; j < d; ++j) {
    int maxDegree = 0;
    for (size_t k = 0; k < p; ++k) {
      if (degrees(k, j) > maxDegree) {
        maxDegree = degrees(k, j);
      }
    }
    NumericMatrix powers1(n, maxDegree+1);
    for (size_t i = 0; i < n; ++i) {
      powers1(i, 0) = 1;
    }
    for (int l = 1; l <= maxDegree; ++l) {
      for (size_t i = 0; i < n; ++i) {
        powers1(i, l) = powers1(i, l-1) * x(i, j);
      }
    }
    powers[j] = powers1;
  }

  for (size_t i = 0; i < n; ++i) {
    for (size_t k = 0; k < p; ++k) {
      v = 1;
      for (size_t j = 0; j < d; ++j) {
        if (degrees(k, j) == 0) continue;
        NumericMatrix pwrs = powers[j];
        v *= pwrs(i, degrees(k, j));
      }
      out(i, k) = v;
    }
  }

  return out;
}


// A function to calculate binomial coefficient
size_t binomialCoefficient(size_t n, size_t k) {
    if (k > n) return 0;
    if (k == 0 || k == n) return 1;

    // Take advantage of symmetry property: C(n, k) == C(n, n-k)
    if (k > n - k) k = n - k;

    size_t result = 1;
    for (size_t i = 0; i < k; ++i) {
        result *= (n - i);
        result /= (i + 1);
    }
    return result;
}



// A recursive function to generate all combinations of exponents
void generateExponents(size_t d, size_t max_degree, std::vector<size_t>& current, size_t current_sum, std::vector<std::vector<size_t>>& result) {
  if (current.size() == d) {
    result.push_back(current);
    return;
  }
  for (size_t i = 0; i <= max_degree - current_sum; ++i) {
    current.push_back(i);
    generateExponents(d, max_degree, current, current_sum + i, result);
    current.pop_back();
  }
}


//' Get exponents of a degree deg polynomial in dim dimensions.
//'
//' @param dimension A positive integer. The dimension of the space.
//' @param degree A nonegative integer. The degreee of the polynomial.
//' @returns A p x d integer matrix. The exponents for the p = (dim + deg choose deg) terms of the polynomial.
//' @export
// [[Rcpp::export]]
IntegerMatrix getMonomialExponents(size_t dimension, size_t degree) {
    std::vector<std::vector<size_t>> result;
    std::vector<size_t> current;
    generateExponents(dimension, degree, current, 0, result);

    // Convert result to Rcpp IntegerMatrix
    size_t n_rows = result.size();
    IntegerMatrix mat(n_rows, dimension);

    for (size_t i = 0; i < n_rows; ++i) {
        for (size_t j = 0; j < dimension; ++j) {
            mat(i, j) = result[i][j];
        }
    }

    return mat;
}
