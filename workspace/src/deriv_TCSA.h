#ifndef DERIV_TCSA_H
#define DERIV_TCSA_H

#include <vector>

template <typename T>
inline std::vector<T> tcsa(const std::vector<T>& state) {
  const T b = T(0.208);

  const T& x = state[0];
  const T& y = state[1];
  const T& z = state[2];

  return {
    sin(y) - b * x,
    sin(z) - b * y,
    sin(x) - b * z
  };
}

#endif
