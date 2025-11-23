options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_polynomials.R")
source("common_integrate.R")
source("common_errorMetrics.R")
source("common_defaults.R")


defaults <- getDefaults()

duration <- 100 # Target duration for reference trajectory in system time.
timeStep <- 2^(-10) # Integration time step for RK4 ODE solver.
warmUpDuration <- 10 # Duration of trajectory to discard at beginning in system time.

testDuration <- 50 # Duration of test part of trajectory in system time.
obs <- 2^(10) # Number of observations for training.
step <- 2^4 # Number of ODE solver time steps between observations (observation time step = timeStep * step).
deg <- 8 # Polynomial Degree for propagator regression.
normalization <- "none" # "none", "diag", "full"

lyapunov <- 0.9064 # Largest Lyapunov exponent for Lorenz 63.

randomSeed <- 1



set.seed(randomSeed)



Rcpp::sourceCpp("propagate_polynomial_64bit.cpp")
propagatePolynomial <- propagate_polynomial_64bit

# source("common_mplapack.R") # 512bit implementation requires MPLAPACK
# Rcpp::sourceCpp("propagate_polynomial_512bit.cpp")
# propagatePolynomial <- propagate_polynomial_512bit



n <- round(duration / timeStep) + 1
nWarmUp <- round(warmUpDuration / timeStep)

integrateSystem <- getIntegrateSystem("L63", "d") # 64bit
state0 <- defaults[["L63"]]$state0
dataAll <- integrateSystem(state0, timeStep, n + nWarmUp)
dataWarm <- dataAll[-seq_len(nWarmUp), ]

time <- dataWarm[,1]
trainDurationMax <- step*obs*timeStep

totalDuration <- trainDurationMax + testDuration
possibleTrainStartTimes <- time[time+totalDuration < time[length(time)]]
if (length(possibleTrainStartTimes) == 0) stop("Could not find possibleTrainStartTimes. Maybe reference trajectory too short?")
trainStartTime <- sample(possibleTrainStartTimes, 1)
testStartTime <- trainStartTime+trainDurationMax
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
  mutate(time = lyapunov*time) |>
  mutate(stateDimIdx = state |> str_extract("\\d") |> as.integer()) |>
  mutate(stateName = case_match(stateDimIdx, 1 ~ "x(t)", 2 ~ "y(t)", 3 ~ "z(t)")) |>
  mutate(
    set =
      factor(set, c("train", "test", "prediction")) |>
      fct_recode("Test Truth" = "test", "Prediction" = "prediction", "Training" = "train")
  ) |>
  ggplot(aes(x = time, y = value, color = set)) +
    geom_line() +
    facet_wrap(vars(stateName), ncol=1, scales="free_y") +
    labs(x = "Time t [Lyapunov times]", y = "Value", color = "") +
    geom_vline(xintercept = vpt, color = "#777") +
    theme(legend.position="bottom")

print(plt)
