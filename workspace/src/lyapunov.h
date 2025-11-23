#ifndef LYAPUNOV_H
#define LYAPUNOV_H

#include <vector>
#include <cmath>
#include <stdexcept>

/// ------------------------------------------------------------
/// 1) LORENZ63 VARIATIONAL SYSTEM
/// ------------------------------------------------------------

/// Computes the Lorenz63 vector field and its variational equations in a single 6D state:
///   state = [ x, y, z, dx, dy, dz ].
/// The first three components are (x, y, z), and the next three are the infinitesimal perturbation
template <typename T>
inline void lorenz63_variational(const std::vector<T>& state, std::vector<T>& dstate) {
    // Lorenz63 parameters
    const T sigma = T(10.0);
    const T rho   = T(28.0);
    const T beta  = T(8.0) / T(3.0);

    // Base trajectory components
    const T x = state[0];
    const T y = state[1];
    const T z = state[2];

    // Lorenz63 vector field
    const T dx = sigma * (y - x);
    const T dy = x * (rho - z) - y;
    const T dz = x * y - beta * z;

    // Jacobian of the Lorenz63 flow at (x, y, z):
    const T J00 = -sigma;
    const T J01 =  sigma;
    const T J02 =  T(0);
    const T J10 =  rho - z;
    const T J11 = -T(1);
    const T J12 = -x;
    const T J20 =  y;
    const T J21 =  x;
    const T J22 = -beta;

    const T dxp = state[3];
    const T dyp = state[4];
    const T dzp = state[5];

    const T ddelta_x = J00 * dxp + J01 * dyp + J02 * dzp;
    const T ddelta_y = J10 * dxp + J11 * dyp + J12 * dzp;
    const T ddelta_z = J20 * dxp + J21 * dyp + J22 * dzp;

    dstate[0] = dx;
    dstate[1] = dy;
    dstate[2] = dz;
    dstate[3] = ddelta_x;
    dstate[4] = ddelta_y;
    dstate[5] = ddelta_z;
}

/// ------------------------------------------------------------
/// 2) TCSA VARIATIONAL SYSTEM
/// ------------------------------------------------------------

/// Computes the TCSA vector field and its variational equations in a single 6D state:
///   state = [ x, y, z, dx, dy, dz ].
/// The first three components are (x, y, z), and the next three are the perturbation (dx, dy, dz).
template <typename T>
inline void tcsa_variational(const std::vector<T>& state, std::vector<T>& dstate) {
    // TCSA parameter
    const T b = T(0.208);

    // Base trajectory components
    const T x = state[0];
    const T y = state[1];
    const T z = state[2];

    // TCSA vector field:
    const T dx = sin(y) - b * x;
    const T dy = sin(z) - b * y;
    const T dz = sin(x) - b * z;

    const T J00 = -b;
    const T J01 =  cos(y);
    const T J02 =  T(0);
    const T J10 =  T(0);
    const T J11 = -b;
    const T J12 =  cos(z);
    const T J20 =  cos(x);
    const T J21 =  T(0);
    const T J22 = -b;

    const T dxp = state[3];
    const T dyp = state[4];
    const T dzp = state[5];

    const T ddelta_x = J00 * dxp + J01 * dyp + J02 * dzp;
    const T ddelta_y = J10 * dxp + J11 * dyp + J12 * dzp;
    const T ddelta_z = J20 * dxp + J21 * dyp + J22 * dzp;

    dstate[0] = dx;
    dstate[1] = dy;
    dstate[2] = dz;
    dstate[3] = ddelta_x;
    dstate[4] = ddelta_y;
    dstate[5] = ddelta_z;
}



/// ------------------------------------------------------------
/// 3) LORENZ96 VARIATIONAL SYSTEM
/// ------------------------------------------------------------

/// Computes the Lorenz96 vector field and its variational equations in a 2d-dimensional state:
///   state = [ x_0, ..., x_{d-1}, dx_0, ..., dx_{d-1} ]
/// The first d components are the base trajectory, and the next d are the tangent perturbations.
template <typename T>
inline void lorenz96_variational(const std::vector<T>& state, std::vector<T>& dstate) {
    const size_t N = state.size();
    if (N % 2 != 0) {
        throw std::invalid_argument("lorenz96_variational: state must have even length (2*d)");
    }

    const size_t d = N / 2;
    const T F = T(8.0);

    dstate.resize(N);

    // Compute base dynamics
    for (size_t i = 0; i < d; ++i) {
        const T& x_ip1 = state[(i + 1) % d];
        const T& x_im1 = state[(i + d - 1) % d];
        const T& x_im2 = state[(i + d - 2) % d];
        const T& x_i   = state[i];

        dstate[i] = (x_ip1 - x_im2) * x_im1 - x_i + F;
    }

    // Compute variational dynamics
    for (size_t i = 0; i < d; ++i) {
        // Indices
        size_t im1 = (i + d - 1) % d;
        size_t im2 = (i + d - 2) % d;
        size_t ip1 = (i + 1) % d;

        // State and perturbation values
        const T& x_im1 = state[im1];
        const T& x_im2 = state[im2];
        const T& x_ip1 = state[ip1];

        const T& dx_ip1 = state[d + ip1];
        const T& dx_im1 = state[d + im1];
        const T& dx_im2 = state[d + im2];
        const T& dx_i   = state[d + i];

        // Derivative of perturbation
        dstate[d + i] =
            (dx_ip1 - dx_im2) * x_im1 +
            (x_ip1 - x_im2) * dx_im1 -
            dx_i;
    }
}




/// Estimates the largest Lyapunov exponent via the tangent-space renormalization method
template <typename T>
T estimate_lyapunov(
    void (*variational)(const std::vector<T>&, std::vector<T>&),
    T dt,
    size_t steps,
    size_t warmup,
    T perturbation_scale,
    const std::vector<T>& state_in,
    const std::vector<T>& direction_in
) {
    if (!variational) {
        throw std::invalid_argument("Variational function pointer must be non-null.");
    }
    if (state_in.size() != direction_in.size()) {
        throw std::invalid_argument("Initial state and direction must be the same length.");
    }
    if (perturbation_scale <= T(0)) {
        throw std::invalid_argument("Perturbation scale must be positive.");
    }
    if (steps <= 0) {
        throw std::invalid_argument("Number of steps must be positive.");
    }
    if (warmup < 0) {
        throw std::invalid_argument("Warmup must be non-negative.");
    }

    const size_t d = state_in.size();

    // Compute the norm of the direction vector
    T norm = std::sqrt(std::inner_product(direction_in.begin(), direction_in.end(), direction_in.begin(), T(0)));
    if (norm == T(0)) {
        throw std::invalid_argument("Initial perturbation direction must be non-zero.");
    }

    // Create combined state vector of size 2d: [state, perturbation]
    std::vector<T> state(2 * d);
    for (size_t i = 0; i < d; ++i) {
        state[i] = state_in[i];
        state[d + i] = (direction_in[i] / norm) * perturbation_scale;
    }

    // Storage for RK4 steps
    std::vector<T> k1(2 * d), k2(2 * d), k3(2 * d), k4(2 * d), temp(2 * d);

    T lyap_sum = T(0);

    for (size_t step = 0; step < warmup + steps; ++step) {
        // RK4 integration
        variational(state, k1);
        for (size_t i = 0; i < 2 * d; ++i)
            temp[i] = state[i] + T(0.5) * dt * k1[i];

        variational(temp, k2);
        for (size_t i = 0; i < 2 * d; ++i)
            temp[i] = state[i] + T(0.5) * dt * k2[i];

        variational(temp, k3);
        for (size_t i = 0; i < 2 * d; ++i)
            temp[i] = state[i] + dt * k3[i];

        variational(temp, k4);
        for (size_t i = 0; i < 2 * d; ++i)
            state[i] += dt / T(6.0) * (k1[i] + T(2) * k2[i] + T(2) * k3[i] + k4[i]);

        // Compute norm of perturbation vector (second half)
        T delta_norm = std::sqrt(std::inner_product(
            state.begin() + d, state.end(), state.begin() + d, T(0)
        ));

        if (step >= warmup) {
            lyap_sum += std::log(delta_norm / perturbation_scale);
        }

        // Renormalize perturbation vector
        if (delta_norm == T(0)) {
            // Reset to default perturbation if collapsed
            state[d] = perturbation_scale;
            for (size_t i = 1; i < d; ++i)
                state[d + i] = T(0);
        } else {
            for (size_t i = 0; i < d; ++i)
                state[d + i] = (state[d + i] / delta_norm) * perturbation_scale;
        }
    }

    return lyap_sum / (static_cast<T>(steps) * dt);
}


#endif // LYAPUNOV_H
