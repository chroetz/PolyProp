options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_polynomials.R")
source("common_errorMetrics.R")



# default parameters
randomSeed <- 1
solverTimeStep <- 2^(-10)
testDuration <- 150
n <- 2^(15)
step <- 2^1
nDeg <- 16
label <- NULL
lyapunov <- NULL

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
if (is.null(lyapunov)) {
  lyapunovInfo <- readInfo(evalDirPath, sprintf("lyapunov_eval_%s_d", systemLabel))
  lyapunov <- lyapunovInfo$lyapunov$mean
}


set.seed(randomSeed)


source("common_mplapack.R")
Rcpp::sourceCpp("truth_and_forecast_mmm_data.cpp")

attractor <- readInfoAndData(truthDirPath, truthName)


state0 <- attractor$data[sample.int(nrow(attractor$data), 1), -1]
tTest <- seq(0, testDuration, by = step*solverTimeStep)[-1]
tTrain <- seq(to = 0, by = step*solverTimeStep, length.out = n)

pt <- proc.time()

degrees <- getMonomialExponents(d, nDeg)

returnData <- integrate_L63_and_propagate_polynomial_512bit_data(
  state0,
  solverTimeStep,
  nStep = step,
  nObs = n,
  nPred = round(testDuration/(solverTimeStep*step)),
  degrees = degrees
)

cat(sprintf("took %.2fs.\n", (proc.time()-pt)[3]))



err <- sqrt(rowSums((returnData$test-returnData$prediction)^2))
result <- bind_cols(time = tTest, error = err)

pltData <-
  bind_rows(
    tibble(time = tTrain, state = returnData$train, set = "train"),
    tibble(time = tTest, state = returnData$test, set = "test"),
    tibble(time = tTest, state = returnData$prediction, set = "prediction")
  ) |>
  mutate(state = as_tibble(state, .name_repair=\(x) as.character(seq_along(x)))) |>
  unnest_wider(state, names_sep = "_") |>
  pivot_longer(starts_with("state_"), names_to="state", values_to="value")



mean0 <- colMeans(attractor$data[,-1])
err0 <- sqrt(mean(rowSums((attractor$data[,-1]-rep(mean0, each=nrow(attractor$data)))^2)))
vpt <- validPredictionTime(err, tTest, 0.5, err0 = err0, lyapunov=lyapunov)

plt <-
  pltData |>
  filter(time %% 2^-5 == 0) |>
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
#    scale_x_continuous(
#      breaks = c(vpt, seq(floor(abs(max(tTrain))/10)*10, testDuration, by = 10))
#    ) +
    theme(legend.position="bottom")

saveGgplotAsTikz(plt, file.path(plotDirPath,  sprintf("L63_example_512bit_n%d_step%d_deg%d_plot.tex", n, step, nDeg)), width=8, heigh=4)
