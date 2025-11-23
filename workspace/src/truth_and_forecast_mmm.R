options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_polynomials.R")


# default parameters
randomSeed <- 1
solverTimeStep <- 2^(-10)
testDuration <- 500
nObs <- 2^(9:10)
nSteps <- c(10, 20)
nDegs <- 1:4
label <- NULL

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set const / derived values
precisionLabel <- "mmm"
systemLabel <- "L63" # only L63 is implemented
testMode <- "sequential" # only sequential test mode is implemented
normalization <- "none" # normalization is not implemented
d <- 3 # System dimension (to define polynomial degrees).
if (is.null(label)) label <- paste(systemLabel, precisionLabel, substr(normalization, 1, 1), substr(testMode, 1, 1), sep="_")
outName <- sprintf("%s_%04d", label, randomSeed)
outDirPath <- file.path(forecastDirPath, label)
truthName <- "L63_md" # This is only used for sampling training initial conditions. Internally, L63_mm is generated.

set.seed(randomSeed)


source("common_mplapack.R")
Rcpp::sourceCpp("truth_and_forecast_mmm.cpp")

attractor <- readInfoAndData(truthDirPath, truthName)

forecastError <-
  expand_grid(nObs, nSteps, nDegs) |>
  mutate(result = rep(list(NULL), n())) |>
  rowid_to_column("idx")

for (n in nObs) for (step in nSteps) {
  cat(sprintf("n: %d, step: %d\n", n, step))

  state0 <- attractor$data[sample.int(nrow(attractor$data), 1), -1]
  tTest <- seq(0, testDuration, by = step*solverTimeStep)[-1]

  for (nDeg in nDegs) {

    cat(sprintf("nDeg: %d... ", nDeg))
    pt <- proc.time()

    degrees <- getMonomialExponents(d, nDeg)
    
    err <- integrate_L63_and_propagate_polynomial_512bit(
      state0,
      solverTimeStep,
      nStep = step,
      nObs = n,
      nPred = round(testDuration/(solverTimeStep*step)),
      degrees = degrees
    )
    colnames(err) <- c("time", "error")
    result <- as_tibble(err)

    idx <- forecastError |> filter(nObs == n, nSteps == step, nDegs == nDeg) |> pull(idx)
    stopifnot(length(idx) == 1)
    forecastError[[idx, "result"]] <- list(result)

    cat(sprintf("took %.2fs.\n", (proc.time()-pt)[3]))
  }
}


info <- lst(
  args,
  randomSeed,
  solverTimeStep,
  testDuration,
  nObs,
  nSteps,
  nDegs,
  precisionLabel,
  systemLabel,
  testMode,
  normalization,
  label,
  truthName
)

writeInfoAndData(info, forecastError, outDirPath, outName)
