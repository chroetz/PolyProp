options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_polynomials.R")
source("common_integrate.R")
source("common_defaults.R")
source("common_errorMetrics.R")


# default parameters
defaults <- getDefaults()
duration <- 10000
timeStep <- 2^(-10)
systemLabel <- "L96D10" # "L63", "TCSA" "L96D<d>"
solverPrecision <- "d" # "s", "d", "m"
warmUpDuration <- NULL
testDuration <- 50
testMode <- "sequential" # "random", "sequential"
obs <- 2^(17)
step <- 2^4
deg <- 6
normalization <- "full" # "none", "diag", "full"
methodPrecision <- "d" # "s", "d", "m"
randomSeed <- 1


# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))


# set derived values
n <- round(duration / timeStep) + 1
if (is.null(warmUpDuration)) {
  warmUpDuration <- duration * 0.1
}
nWarmUp <- round(warmUpDuration / timeStep)


set.seed(randomSeed)


integrateSystem <- getIntegrateSystem(systemLabel, solverPrecision)
state0 <- defaults[[systemLabel]]$state0
dataAll <- integrateSystem(state0, timeStep, n + nWarmUp)
dataWarm <- dataAll[-seq_len(nWarmUp), ]


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


time <- dataWarm[,1]
trainDurationMax <- step*obs*timeStep

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
trajTrain <- dataWarm[time >= trainStartTime & time<trainStartTime+trainDurationMax, ]
nTrajTrain <- nrow(trajTrain)
trajTest <- dataWarm[time >= testStartTime & time<testStartTime+testDuration, ]
testInitial <- dataWarm[which.max(time >= testStartTime)-1, ]


xTrain <- trajTrain[seq(nTrajTrain-(obs-1)*step, nTrajTrain, step), -1]
stopifnot(nrow(xTrain) == obs)
xTest <- trajTest[seq(step, nrow(trajTest), step), -1]
tTest <- trajTest[seq(step, nrow(trajTest), step), 1]
stopifnot(abs((tTest[1] - testInitial[1]) - (timeStep * step)) < sqrt(.Machine$double.eps))
xInitial <- testInitial[-1]
tTest <- tTest - tTest[1] + step * timeStep
d <- ncol(xTest)
  

cat(sprintf("nDeg: %d... ", deg))
pt <- proc.time()
degrees <- getMonomialExponents(d, deg)
prediction <- propagatePolynomial(
  xTrain,
  xInitial,
  nPred = nrow(xTest),
  degrees = degrees,
  normalization = normalization
)
cat(sprintf("took %.2fs.\n", (proc.time()-pt)[3]))


err <- sqrt(rowSums((xTest-prediction)^2))
result <- bind_cols(time = tTest, error = err)

mean0 <- colMeans(dataWarm[,-1])
err0 <- sqrt(mean(rowSums((dataWarm[,-1]-rep(mean0, each=nrow(dataWarm)))^2)))

vpt <- validPredictionTime(
  err = err, 
  time = tTest, 
  thresh = 0.5,  # VPT_{0.5}
  err0 = err0, # sigma
  lyapunov = 1 # 1 -> system time units; max lyapunov exponent -> Lyapunov times
)

print(result)

print(vpt)



