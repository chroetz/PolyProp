options(tidyverse.quiet = TRUE)
library(tidyverse)
library(gt)
source("common_paths.R")
source("common_io.R")
source("common_polynomials.R")
source("common_errorMetrics.R")


# default parameters
randomSeed <- 1
truthName <- "L63_dd"
testDuration <- 50
testMode <- "sequential" # "random", "sequential"
n <- 2^(13)
step <- 2^2
nDeg <- 5
normalization <- "none" # "none", "diag", "full"
label <- NULL
lyapunov <- NULL

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived values
truth <- readInfoAndData(truthDirPath, truthName)
systemLabel <- truth$info$systemLabel
if (is.null(label)) label <- paste("fit", truthName, substr(normalization, 1, 1), sep="_")

if (is.null(lyapunov)) {
  lyapunovInfo <- readInfo(evalDirPath, sprintf("lyapunov_eval_%s_d", systemLabel))
  lyapunov <- lyapunovInfo$lyapunov$mean
}


set.seed(randomSeed)


time <- truth$data[,1]
timeStep <- truth$info$timeStep
trainDurationMax <- step*n*timeStep

totalDuration <- trainDurationMax + testDuration
possibleTrainStartTimes <- time[time+totalDuration < time[length(time)]]
if (length(possibleTrainStartTimes) == 0) stop("Could not find possibleTrainStartTimes. Maybe truth trajectory too short?")
trainStartTime <- sample(possibleTrainStartTimes, 1)
testStartTime <- trainStartTime+trainDurationMax

trajTrain <- truth$data[time >= trainStartTime & time<trainStartTime+trainDurationMax, ]
nTrajTrain <- nrow(trajTrain)
trajTest <- truth$data[time >= testStartTime & time<testStartTime+testDuration, ]
testInitial <- truth$data[which.max(time >= testStartTime)-1, ]

idxTrain <- seq(nTrajTrain-(n-1)*step, nTrajTrain, step)
xTrain <- trajTrain[idxTrain, -1]
tTrain <- trajTrain[idxTrain, 1]
tTrain <- tTrain - tTrain[length(tTrain)]
stopifnot(nrow(xTrain) == n)
xTest <- trajTest[seq(step, nrow(trajTest), step), -1]
tTest <- trajTest[seq(step, nrow(trajTest), step), 1]
stopifnot(tTest[1] - testInitial[1] == timeStep * step)
xInitial <- testInitial[-1]
tTest <- tTest - tTest[1] + step * timeStep
d <- ncol(xTest)


degrees <- getMonomialExponents(d, nDeg)



Rcpp::sourceCpp("fit_polynomial_64bit.cpp")
fitPolynomial <- fit_polynomial_64bit

coeff <- fitPolynomial(
  xTrain,
  degrees = degrees,
  normalization = normalization
)

colnames(coeff) <- c("x", "y", "z")
exponentsToMonomialString <- function(degs) {
  stopifnot(length(degs) == 3)
  vars <- c("x", "y", "z")
  text <- paste(vars, "^{", degs, "}", sep="")
  text[degs == 0] <- ""
  text[degs == 1] <- vars[degs == 1]
  if (all(degs == 0)) text <- "1"
  paste0("$", paste(text, collapse=""), "$")
}
monomial <- apply(degrees, 1, exponentsToMonomialString)
colnames(degrees) <- c("degX", "degY", "degZ")
tableData <- bind_cols(monomial = monomial, as_tibble(coeff), as_tibble(degrees))

tbl <-
  tableData |>
  mutate(degSum = degX + degY + degZ) |>
  arrange(degSum, desc(degX), desc(degY), desc(degZ)) |>
  select(-c(degX, degY, degZ)) |>
  rowid_to_column("id") |>
  mutate(monomial = I(monomial)) |>
  gt() |>
  opt_row_striping(row_striping = TRUE) |>
  opt_vertical_padding(scale = 0) |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    id = "#",
    degSum = "deg.",
    x = I("$x(t + \\Delta\\!t)$"),
    y = I("$y(t + \\Delta\\!t)$"),
    z = I("$z(t + \\Delta\\!t)$"),
    monomial = "Monomial"
  ) |>
  cols_move_to_start(c(id, degSum, monomial)) |>
  tab_spanner(
    label = "Coefficients for Output",
    columns = c(x,y,z)
  ) |>
  tab_spanner(
    label = "Input",
    columns = c(id, degSum, monomial)
  ) |>
  fmt_scientific(
    columns = c(x,y,z),
    n_sigfig = 16,
    exp_style = "E"
  )

writeTableAsLatex(tbl, file.path(tableDirPath, sprintf("%s_n%d_step%d_deg%d_table.tex", label, n, step, nDeg)))



Rcpp::sourceCpp("propagate_polynomial_64bit.cpp")
propagatePolynomial <- propagate_polynomial_64bit

prediction <- propagatePolynomial(
  xTrain,
  xInitial,
  nPred = nrow(xTest),
  degrees = degrees,
  normalization = normalization
)

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



mean0 <- colMeans(truth$data[,-1])
err0 <- sqrt(mean(rowSums((truth$data[,-1]-rep(mean0, each=nrow(truth$data)))^2)))
vpt <- validPredictionTime(err, tTest, 0.5, err0 = err0, lyapunov=lyapunov)

timeMax <- 25
timeMin <- -30
plt <-
  pltData |>
  mutate(time = lyapunov*time) |>
  filter(time <= timeMax) |>
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
  scale_x_continuous(breaks = seq(timeMin, timeMax, by = 10))

saveGgplotAsTikz(plt, file.path(plotDirPath,  sprintf("%s_n%d_step%d_deg%d_plot.tex", label, n, step, nDeg)), width=8, heigh=4)

