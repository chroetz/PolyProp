options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_polynomials.R")
source("common_integrate.R")
source("common_errorMetrics.R")
source("common_defaults.R")


defaults <- getDefaults()
duration <- 100
timeStep <- 2^(-10)
systemLabel <- "L63" # "L63", "TCSA"
solverPrecision <- "m" # "s", "d", "m"
warmUpDuration <- NULL

testDuration <- 50
testMode <- "sequential" # "random", "sequential"
obs <- 2^(10)
step <- 2^4
deg <- 9
normalization <- "none" # "none", "diag", "full"
methodPrecision <- "m" # "s", "d", "m"
randomSeed <- 1

lyapunov <- NULL
if (is.null(lyapunov)) {
  lyapunovInfo <- readInfo(evalDirPath, sprintf("lyapunov_eval_%s_d", systemLabel))
  lyapunov <- lyapunovInfo$lyapunov$mean
}

set.seed(randomSeed)


n <- round(duration / timeStep) + 1
if (is.null(warmUpDuration)) {
  warmUpDuration <- duration * 0.1
}
nWarmUp <- round(warmUpDuration / timeStep)


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


idxTrain <- seq(nTrajTrain-(obs-1)*step, nTrajTrain, step)
xTrain <- trajTrain[idxTrain, -1]
tTrain <- trajTrain[idxTrain, 1]
tTrain <- tTrain - tTrain[length(tTrain)]
stopifnot(nrow(xTrain) == obs)
idxTest <- seq(step, nrow(trajTest), step)
xTest <- trajTest[idxTest, -1]
tTest <- trajTest[idxTest, 1]
stopifnot(abs((tTest[1] - testInitial[1]) - (timeStep * step)) < sqrt(.Machine$double.eps))
xInitial <- testInitial[-1]
tInitial <- testInitial[1]
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

pltData <-
  bind_rows(
    tibble(time = tTrain, state = xTrain, set = "train"),
    tibble(time = tTest, state = xTest, set = "test"),
    tibble(time = tTest, state = prediction, set = "prediction")
  ) |>
  mutate(state = as_tibble(state, .name_repair=\(x) as.character(seq_along(x)))) |>
  unnest_wider(state, names_sep = "_") |>
  pivot_longer(starts_with("state_"), names_to="state", values_to="value")



mean0 <- colMeans(dataWarm[,-1])
err0 <- sqrt(mean(rowSums((dataWarm[,-1]-rep(mean0, each=nrow(dataWarm)))^2)))
vpt <- validPredictionTime(err, tTest, 0.5, err0 = err0, lyapunov=lyapunov)

plt <-
  pltData |>
  filter(time <= 50) |>
  mutate(time = lyapunov*time) |>
  mutate(stateDimIdx = state |> str_extract("\\d") |> as.integer()) |>
  mutate(stateName = case_match(stateDimIdx, 1 ~ "$x(t)$", 2 ~ "$y(t)$", 3 ~ "$z(t)$")) |>
  mutate(
    set =
      factor(set, c("train", "test", "prediction")) |>
      fct_recode("Test Truth" = "test", "Prediction" = "prediction", "Training" = "train")
  ) |>
  ggplot(aes(x = time, y = value, color = set)) +
    geom_line() +
    facet_wrap(vars(stateName), ncol=1, scales="free_y") +
    labs(x = "Time $t$ [Lyapunov times]", y = "Value", color = "") +
    geom_vline(xintercept = vpt, color = "#777") +
    theme(legend.position="bottom") +
    scale_x_continuous(breaks = seq(-10, 50, by = 10))

saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_exmaple_plot.tex"), width=8, heigh=4)

