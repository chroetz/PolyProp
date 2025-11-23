options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_polynomials.R")


# default parameters
randomSeed <- 1
truthName <- "L63_dd"
testDuration <- 50
testMode <- "sequential" # "random", "sequential"
nObs <- 2^(10)
nSteps <- 2^5
nDegs <- c(1:8)
normalization <- "none" # "none", "diag", "full"
methodPrecision <- "d" # "s", "d", "m"
label <- NULL
noiseLevels <- 10^c(-10,-8,-6,-4,-2,0)

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived values
truth <- readInfoAndData(truthDirPath, truthName)
systemLabel <- truth$info$systemLabel
precisionLabel <- paste0(truth$info$precisionLabel, methodPrecision)
if (is.null(label)) label <- paste("noisy", systemLabel, precisionLabel, substr(normalization, 1, 1), substr(testMode, 1, 1), sep="_")
outName <- sprintf("%s_%04d", label, randomSeed)
outDirPath <- file.path(forecastDirPath, label)


set.seed(randomSeed)


if (methodPrecision == "s") {
  source("common_openblas.R")
  Rcpp::sourceCpp("propagate_polynomial_32bit.cpp")
  propagatePolynomial <- propagate_polynomial_32bit
} else if (methodPrecision == "d") {
  Rcpp::sourceCpp("propagate_polynomial_64bit.cpp")
  propagatePolynomial <- propagate_polynomial_64bit
} else if (methodPrecision == "m") {
  source("common_mplapack.R")
  Rcpp::sourceCpp("propagate_polynomial_512bit.cpp")
  propagatePolynomial <- propagate_polynomial_512bit
} else {
  stop("Unknown methodPrecision: ", methodPrecision)
}

truthSd <- sd(truth$data[,-1])

time <- truth$data[,1]
timeStep <- truth$info$timeStep
trainDurationMax <- max(nSteps)*max(nObs)*timeStep

if (testMode == "random") {
  possibleTrainStartTimes <- time[time+trainDurationMax < time[length(time)]]
  if (length(possibleTrainStartTimes) == 0) stop("Could not find possibleTrainStartTimes. Maybe truth trajectory too short?")
  trainStartTime <- sample(possibleTrainStartTimes, 1)
  possibleTestStartTimes <- time[time > time[1] & time+testDuration < time[length(time)]]
  if (length(possibleTestStartTimes) == 0) stop("Could not find possibleTestStartTimes. Maybe truth trajectory too short?")
  testStartTime <- sample(possibleTestStartTimes, 1)
} else if (testMode == "sequential") {
  totalDuration <- trainDurationMax + testDuration
  possibleTrainStartTimes <- time[time+totalDuration < time[length(time)]]
  if (length(possibleTrainStartTimes) == 0) stop("Could not find possibleTrainStartTimes. Maybe truth trajectory too short?")
  trainStartTime <- sample(possibleTrainStartTimes, 1)
  testStartTime <- trainStartTime+trainDurationMax
} else stop("Unknown testMode ", testMode)
trajTrain <- truth$data[time >= trainStartTime & time<trainStartTime+trainDurationMax, ]
nTrajTrain <- nrow(trajTrain)
trajTest <- truth$data[time >= testStartTime & time<testStartTime+testDuration, ]
testInitial <- truth$data[which.max(time >= testStartTime)-1, ]



info <- lst(
  args,
  randomSeed,
  truthName,
  testDuration,
  testMode,
  nObs,
  nSteps,
  nDegs,
  noiseLevels,
  normalization,
  methodPrecision,
  outName,
  systemLabel,
  precisionLabel,
  trainDurationMax,
  trainStartTime,
  testStartTime,
  testInitial
)

cat("Run forecast with following info:\n")
printInfo(info)



forecastError <-
  expand_grid(nObs, nSteps, noiseLevels, nDegs) |>
  mutate(result = rep(list(NULL), n())) |>
  rowid_to_column("idx")

for (n in nObs) for (step in nSteps) {
  cat(sprintf("n: %d, step: %d\n", n, step))

  xTrain <- trajTrain[seq(nTrajTrain-(n-1)*step, nTrajTrain, step), -1]
  stopifnot(nrow(xTrain) == n)
  xTest <- trajTest[seq(step, nrow(trajTest), step), -1]
  tTest <- trajTest[seq(step, nrow(trajTest), step), 1]
  stopifnot(tTest[1] - testInitial[1] == timeStep * step)
  xInitial <- testInitial[-1]
  tTest <- tTest - tTest[1] + step * timeStep
  d <- ncol(xTest)

  for (noiseLevel in noiseLevels) {

    cat(sprintf("noiseLevel: %g. ", noiseLevel))
    if (noiseLevel == 0) {
      xTrainNoised <- xTrain
    } else {
      xTrainNoised <- xTrain + rnorm(length(xTrain), sd = noiseLevel*truthSd)
    }

    for (nDeg in nDegs) {

      cat(sprintf("nDeg: %d... ", nDeg))
      pt <- proc.time()

      degrees <- getMonomialExponents(d, nDeg)
      prediction <- propagatePolynomial(
        xTrainNoised,
        xInitial,
        nPred = nrow(xTest),
        degrees = degrees,
        normalization = normalization
      )

      err <- sqrt(rowSums((xTest-prediction)^2))
      result <- bind_cols(time = tTest, error = err)

      idx <- forecastError |> filter(nObs == n, nSteps == step, noiseLevels == noiseLevel, nDegs == nDeg) |> pull(idx)
      stopifnot(length(idx) == 1)
      forecastError[[idx, "result"]] <- list(result)

      cat(sprintf("took %.2fs.\n", (proc.time()-pt)[3]))
    }
  }
}


writeInfoAndData(info, forecastError, outDirPath, outName) |> print()
