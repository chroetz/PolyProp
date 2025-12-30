# Machine-Precision Prediction of Low-Dimensional Chaotic Systems from Noise-Free Data

This repository contains all scripts and instructions needed to replicate the simulation study in the article *Machine-Precision Prediction of Low-Dimensional Chaotic Systems from Noise-Free Data* by Christof Schötz and Niklas Boers.

---

## 1. Prerequisites

1. **Directory Layout**
   Make sure you have the following top-level folders (adjust `some/path/` as needed):

   ```
   some/path/PolyProp/workspace
   some/path/PolyProp/data
   ~/MPLAPACK
   ~/OpenBLAS
   ```

   * **`workspace`**: Contains R source code. Plots and LaTeX tables will be generated here.
      - **`img`**: results reported as images
      - **`key`**: results reported as key-value pairs
      - **`tbl`**: results reported as LaTeX tables
      - **`src`**: source code

   * **`data`**: Will store generated data.
      - **`truth`**: long ground truth trajectories for each system
      - **`solverError`**: error trajectories of ODE solvers
      - **`forecast`**: error trajectories of forecasts created by polynomial regression propagator
         + **`<sys>_<prec>_<norm>_<mode>`**: system `<sys>` with system-data-forecast precision `<prec>` (s: single, d: double, m: multi) with normalization `<norm>` (n: none, d: diagonal, f: full) in mode `<mode>` (s: sequential, r: random)
      - **`evaluation`**: error metrics such as VPT calculated from error trajectories

   * **`~/MPLAPACK`**: Local installation of MPLAPACK version 2.0.1.

   * **`~/OpenBLAS`**: Local installation of OpenBLAS version 0.3.29.

   > **Note**: If your folder structure differs, edit
   > `workspace/src/common_path.R`
   > `workspace/src/common_mplapack.R`
   > `workspace/src/common_openblas.R`
   > to point to the correct locations before proceeding.

2. **R and Required Packages**

   R, at least version 4.3, https://www.r-project.org/

   From within `workspace/src/`, run:

   ```bash
   Rscript _required.R
   ```

   This will install the required R packages.

3. **Slurm**

   A Slurm-managed cluster is assumed, https://slurm.schedmd.com/

4. **MPLAPACK**

   MPLAPACK version 2.0.1, https://github.com/nakatamaho/mplapack

5. **OpenBLAS**

   OpenBLAS version 0.3.29, http://www.openmathlib.org/OpenBLAS

6. **Compiler**

   On Windows, you may need a mingw-w64/bin folder in your PATH environment variable. Download it e.g., from https://winlibs.com/

7. **LaTeX**

   For creating plots and LaTeX tables, a TeX distribution such as TeX Live, https://www.tug.org/texlive/

8. **Julia**

   *only for Taylor Integration (extremely high precision ODE solver; standard experiments use RK4 instead):* Julia version 1.11.1, https://julialang.org/

---

## 2. Example

See `workspace/src/_example.R` for an example script that creates a Lorenz 63 trajectory, fits a polynomial propagator, makes a prediction, and plots the result.

---

## 3. Workflow 

Run all code by calling `Rscript run_<name>.R <x> [<from:to>]`, where `<x>` is `c` on a slurm managed cluster (submits slurm jobs; might need to edit `src/run_*` and `src/common_run.R` to use the right slurm options such as the QOS name), `s` for local sequential execution, or `p` for local parallel execution. Some scripts have an additional argument of the form `from:to` where `from` and `to` are natural numbers and describe the range of random seeds the scripts are run with.

```bash
cd workspace/src
# Generate Ground-Truth Trajectories
Rscript run_truth.R c 
# Wait until all jobs are finished!
Rscript run_truth_32bit.R c 
# Wait until all jobs are finished!
# Estimate Largest Lyapunov Exponents
Rscript run_lyapunov.R c
# Measure ODE-Solver Errors
Rscript run_solverError.R c 1:100
# Run Forecasts and Produce Forecast-Error Time Series
Rscript run_forecast_main_fast.R c 1:100
Rscript run_forecast_main_slow.R c 1:100
Rscript run_forecast_TCSA_extra.R c 1:100
Rscript run_forecast_L63_extra.R c 1:100
Rscript run_forecast_noisy.R c 1:100
# Wait until all jobs are finished!
# Calculate Valid Prediction Times
Rscript run_solverError_evaluate.R c
Rscript run_evaluate.R c
# Wait until all jobs are finished!
# Calculate Statistics of Error Metrics (mean VPT)
Rscript run_errorStats.R c
# Wait until all jobs are finished!
# Create Plots and LaTeX Tables (requires LaTeX installation)
Rscript run_report.R c
```

Additional experiments with extreme precision solvers:

```bash
cd workspace/src # workspace/src
# Generate Ground-Truth Trajectories
Rscript run_truth_y.R c 
cd truth_xd # workspace/src/truth_xd
sbatch submit_L63_xd_chain.sh # needs Julia
# Wait until all jobs are finished!
cd .. # workspace/src
Rscript truth_x_combine.R # Note: Does not use slurm.
# Measure ODE-Solver Errors
Rscript run_solverError_y.R c 1:100
cd solverError_x # workspace/src/solverError_x
sbatch submit_xdx.sh # needs Julia
cd .. # workspace/src
# Run Forecasts and Produce Forecast-Error Time Series
Rscript run_forecast_main_fast_L63_xdm.R c 1:100
Rscript run_forecast_main_fast_L63_ydm.R c 1:100
# Wait until all jobs are finished!
# Calculate Valid Prediction Times
Rscript run_solverError_y_evaluate.R c
Rscript run_evaluate_xy.R c
# Wait until all jobs are finished!
# Calculate Statistics of Error Metrics (mean VPT)
Rscript run_errorStats.R c
# Wait until all jobs are finished!
# Create Plots and LaTeX Tables (requires LaTeX installation)
Rscript run_report_xy.R c
```

---

## 4. Outputs

File formats:

- larger data files (`.feather`) are stored in the Feather File Format (Apache Arrow),  https://arrow.apache.org/docs/python/feather.html
- metadata is stored in `.json` files, https://www.json.org/json-en.html
- results that require less memory are stored in `.csv` files, https://en.wikipedia.org/wiki/Comma-separated_values
- reporting is done in LaTeX source code (`.tex`) for tables, in PNG image files (`.png`), or in PDFs (`.pdf`) created via tikz: https://tikz.dev/

**Note:** This repository only contains the large data files of the first out of 100 repetitions for forecast error trajectories. But all metadata files and evaluations are contained.
