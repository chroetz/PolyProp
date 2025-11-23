#ifndef DERIV_L96_H
#define DERIV_L96_H

#include <vector>

template <typename T>
inline std::vector<T> lorenz96(const std::vector<T>& state) {
  const size_t d = state.size();
  const T F = T(8.0);

  std::vector<T> dxdt(d);

  for (size_t i = 0; i < d; ++i) {
    const T& x_im1 = state[(i + d - 1) % d];   // x_{i-1}
    const T& x_im2 = state[(i + d - 2) % d];   // x_{i-2}
    const T& x_ip1 = state[(i + 1) % d];       // x_{i+1}

    dxdt[i] = (x_ip1 - x_im2) * x_im1 - state[i] + F;
  }

  return dxdt;
}

#endif
