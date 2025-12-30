#ifndef RK4_Y_3D_H
#define RK4_Y_3D_H

#include <vector>
#include <array>
#include <functional>

template <typename T, typename Func>
std::vector<std::array<T, 4>> rk4_y_3d(const std::array<T, 3>& start, T dt, size_t nOut, size_t nSkip, Func f) {
  T t = T(0.0);
  std::array<T, 3> state = start;
  std::vector<std::array<T, 4>> result(nOut);

  const T dt_half = dt * T(0.5);
  const T dt_sixth = dt / T(6.0);

  std::array<T, 3> temp1, temp2, temp3;

  for (size_t i = 0; i < nOut; ++i) {
    result[i][0] = t;
    result[i][1] = state[0];
    result[i][2] = state[1];
    result[i][3] = state[2];

    for (size_t s = 0; s < nSkip; ++s) {
      auto k1 = f(state);

      for (size_t j = 0; j < 3; ++j)
        temp1[j] = state[j] + dt_half * k1[j];
      auto k2 = f(temp1);

      for (size_t j = 0; j < 3; ++j)
        temp2[j] = state[j] + dt_half * k2[j];
      auto k3 = f(temp2);

      for (size_t j = 0; j < 3; ++j)
        temp3[j] = state[j] + dt * k3[j];
      auto k4 = f(temp3);

      for (size_t j = 0; j < 3; ++j)
        state[j] += dt_sixth * (k1[j] + T(2) * k2[j] + T(2) * k3[j] + k4[j]);

      t += dt;
    }
  }

  return result;
}

#endif
