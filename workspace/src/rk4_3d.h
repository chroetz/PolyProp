#ifndef RK4_3D_H
#define RK4_3D_H

#include <vector>
#include <functional>

template <typename T, typename Func>
std::vector<std::vector<T>> rk4_3d(const std::vector<T>& start, T dt, size_t nSteps, Func f) {
  T t = T(0.0);
  std::vector<T> state = start;
  std::vector<std::vector<T>> result(nSteps, std::vector<T>(4)); // t, x, y, z

  for (size_t i = 0; i < nSteps; ++i) {
    result[i][0] = t;
    result[i][1] = state[0];
    result[i][2] = state[1];
    result[i][3] = state[2];

    auto k1 = f(state);

    std::vector<T> temp1(3), temp2(3), temp3(3);
    for (size_t j = 0; j < 3; ++j)
      temp1[j] = state[j] + T(0.5) * dt * k1[j];
    auto k2 = f(temp1);

    for (size_t j = 0; j < 3; ++j)
      temp2[j] = state[j] + T(0.5) * dt * k2[j];
    auto k3 = f(temp2);

    for (size_t j = 0; j < 3; ++j)
      temp3[j] = state[j] + dt * k3[j];
    auto k4 = f(temp3);

    for (size_t j = 0; j < 3; ++j)
      state[j] += dt / T(6.0) * (k1[j] + T(2) * k2[j] + T(2) * k3[j] + k4[j]);

    t += dt;
  }

  return result;
}

#endif
