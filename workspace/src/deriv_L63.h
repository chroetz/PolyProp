#ifndef DERIV_L63_H
#define DERIV_L63_H

#include <vector>

template <typename T>
inline std::vector<T> lorenz63(const std::vector<T>& state) {
  const T sigma = T(10.0);
  const T rho   = T(28.0);
  const T beta  = T(8.0) / T(3.0);

  const T& x = state[0];
  const T& y = state[1];
  const T& z = state[2];

  return {
    sigma * (y - x),
    x * (rho - z) - y,
    x * y - beta * z
  };
}

#endif
