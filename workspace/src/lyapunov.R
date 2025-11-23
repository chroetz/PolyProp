options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")


# default parameters
randomSeed <- 1
truthName <- "L96D5_dd"
label <- NULL
nReps <- 1e2
timeStep <- 2^(-10)
steps <- 1e5
warmupSteps <- 0
perturbationScale <- 1e-8
quantileProbs <- c(0, 0.25, 0.5, 0.75, 1)


# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))


# set derived values
systemLabel <- str_extract(truthName, "^[^_]*")
solverPrecision <- "d"
outName <- paste("lyapunov_eval", systemLabel, solverPrecision, sep="_")
outFilePath <- file.path(evalDirPath, paste0(outName, ".json"))
truth <- readInfoAndData(truthDirPath, truthName)
set.seed(randomSeed)


Rcpp::sourceCpp("lyapunov_64bit.cpp")
if (stringr::str_detect(systemLabel, "^L96D(\\d+)$")) {
  d <- as.integer(stringr::str_sub(systemLabel, start=5))
  estimateLyapunov <- estimate_lyapunov_L96_64bit
} else {
  d <- 3
  estimateLyapunov <- switch(
    systemLabel,
    L63 = estimate_lyapunov_L63_64bit,
    TCSA = estimate_lyapunov_TCSA_64bit,
    stop("Unknown systemLabel ", systemLabel)
  )
}
lyapunovEstimates <- replicate(nReps, {
  x0 <- truth$data[sample.int(nrow(truth$data), 1), -1]
  delta0 <- rnorm(d)
  estimateLyapunov(timeStep, steps, warmupSteps, perturbationScale, x0, delta0)
})

lyapunov <- lst(
  mean = mean(lyapunovEstimates),
  quantiles = quantile(lyapunovEstimates, quantileProbs),
  sd = sd(lyapunovEstimates),
  n = length(lyapunovEstimates),
  confInt95 = mean + c(-1,1)*qnorm(0.975)*sd/sqrt(n),
)


info <- lst(
  args,
  randomSeed,
  truthName,
  systemLabel,
  solverPrecision,
  label,
  nReps,
  timeStep,
  steps,
  perturbationScale,
  lyapunov
)


writeInfo(info, outFilePath)

