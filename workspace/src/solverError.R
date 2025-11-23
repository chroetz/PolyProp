options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_integrate.R")


# default parameters
randomSeed <- 1
truthName <- "L96D5_ds"
testDuration <- 50
solverPrecision <- "d" # "s", "d", "m"
timeStep <- 2^(-10)
label <- NULL
nReps <- 10


# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))


# set derived values
truth <- readInfoAndData(truthDirPath, truthName)
systemLabel <- truth$info$systemLabel
if (is.null(label)) label <- paste("solverError", truthName, solverPrecision, sep="_")
outName <- sprintf("%s_%04d", label, randomSeed)
outDirPath <- solverErrorDirPath


set.seed(randomSeed)


integrateSystem <- getIntegrateSystem(systemLabel, solverPrecision)
testTime <- seq(0, testDuration, by = timeStep)

n <- round(testDuration / timeStep)
solverError <- vapply(
  seq_len(nReps),
  \(i) {
    cat(i, "\n")
    sampledStartIdx <- sample.int(nrow(truth$data) - n - 1, 1)
    state0 <- truth$data[sampledStartIdx, -1]
    time0 <- truth$data[sampledStartIdx, 1]
    target <- truth$data[sampledStartIdx:(sampledStartIdx+n), ]
    forecast <- integrateSystem(state0, timeStep, n + 1)
    sqrt(rowSums((target[,-1] - forecast[,-1])^2))
  },
  double(n+1)
)


info <- lst(
  args,
  randomSeed,
  truthName,
  testDuration,
  solverPrecision,
  timeStep,
  label,
  nReps,
  systemLabel
)


writeInfoAndData(info, solverError, outDirPath, outName)
