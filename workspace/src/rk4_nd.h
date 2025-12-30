#ifndef RK4_ND_H
#define RK4_ND_H

#include <vector>
#include <functional>

template <typename T, typename Func>
std::vector<std::vector<T>> rk4_nd(const std::vector<T>& start, T dt, size_t nOut, Func f) {
  const size_t d = start.size();
  T t = T(0.0);
  std::vector<T> state = start;
  std::vector<std::vector<T>> result(nOut, std::vector<T>(d + 1)); // t + d components

  for (size_t i = 0; i < nOut; ++i) {
    result[i][0] = t;
    for (size_t j = 0; j < d; ++j)
      result[i][j + 1] = state[j];

    auto k1 = f(state);

    std::vector<T> temp1(d), temp2(d), temp3(d);
    for (size_t j = 0; j < d; ++j)
      temp1[j] = state[j] + T(0.5) * dt * k1[j];
    auto k2 = f(temp1);

    for (size_t j = 0; j < d; ++j)
      temp2[j] = state[j] + T(0.5) * dt * k2[j];
    auto k3 = f(temp2);

    for (size_t j = 0; j < d; ++j)
      temp3[j] = state[j] + dt * k3[j];
    auto k4 = f(temp3);

    for (size_t j = 0; j < d; ++j)
      state[j] += dt / T(6.0) * (k1[j] + T(2) * k2[j] + T(2) * k3[j] + k4[j]);

    t += dt;
  }

  return result;
}

#endif
