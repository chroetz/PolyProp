options(tidyverse.quiet = TRUE)
library(tidyverse)
library(gt)
source("common_paths.R")
source("common_io.R")
source("common_defaults.R")



# default parameters
errorMetric <- "vpt0.5"
defaults <- getDefaults()
errorMetricLabel <- "VPT"

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived and const values
errorStatsFilePath <- file.path(evalDirPath, sprintf("errorStats_%s.csv", errorMetric))
errorStats <- read_csv(errorStatsFilePath)
precisionToBit <- c(s = "32 bit", d = "64 bit", m = "512 bit")


filePaths <- list.files(evalDirPath, pattern="^solverError_eval_.*\\.json", full.names=TRUE)
solverError <-
  lapply(filePaths, \(filePath) {
    info <- readInfo(filePath = filePath)
    tibble(
      system = info$systemLabel,
      solverPrecision = info$solverPrecision,
      truthPrecision = str_sub(info$truthName, start=-2, end=-1),
      vpt = info$vpt$mean
    )
  }) |>
  bind_rows()

bestResults <-
  errorStats |>
  filter(system == "L63",  nSteps != 1, testMode == "sequential") |> # exclude estimation at the solver time step for L63 as this is polynomial of degree 8, so that polynomial propagator regression might have an unfair advantage in this case
  summarise(
    deg = nDegs[which.max(mean)],
    step = nSteps[which.max(mean)],
    .by = c(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs)
  ) |>
  left_join(
    errorStats, join_by(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs, deg == nDegs, step == nSteps)
  ) |>
  mutate(meanRound = round(mean)) |>
  filter(mean >= max(ci95Lower), .by = c(systemPrecision, dataPrecision, methodPrecision)) |>
  filter(nObs == min(nObs), .by = c(systemPrecision, dataPrecision, methodPrecision)) |>
  arrange(mean)

solverVptL96 <-
  solverError |>
  filter(startsWith(system, "L96"))

bbest <-
  errorStats |>
   filter(
    startsWith(system, "L96"),
    testMode == "sequential",
    methodPrecision == "d",
  ) |>
  filter(nObs == 2^17, nSteps != 1, nSteps == 8) |>
  summarise(
    deg = nDegs[which.max(mean)],
    step = nSteps[which.max(mean)],
    .by = c(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs)
  ) |>
  left_join(
    errorStats, join_by(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs, deg == nDegs, step == nSteps)
  ) |>
  mutate(truthPrecision = paste0(systemPrecision, dataPrecision)) |>
  left_join(solverVptL96 |> rename(solverVpt = vpt), join_by(truthPrecision, system)) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, nObs, deg, step, mean, sd, n, ci95Lower, ci95Upper, solverVpt) |>
  mutate(ok = ci95Upper >= solverVpt)


bbestLong <-
  bbest |>
  mutate(dimension = str_sub(system, 5)) |>
  pivot_longer(
    cols = c(mean, solverVpt),
    names_to = "method",
    values_to = "vpt"
  )  |>
  mutate(method = recode(method, "mean" = "PolyProp", "solverVpt" = "RK4 ODE Solver")) |>
  mutate(
    ci95Lower = ifelse(method == "PolyProp", ci95Lower, NA),
    ci95Upper = ifelse(method == "PolyProp", ci95Upper, NA)
  ) |>
  mutate(precision = ifelse(systemPrecision == "s", "32-bit system", "64-bit system rounded to 32-bit data"))


plt <- bbestLong |>
  ggplot(aes(x = dimension, y = vpt, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = ci95Lower, ymax = ci95Upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(vars(precision)) +
  labs(
    y = "Valid Prediction Time [Lyapunov times]",
    x = "Dimension",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

saveGgplotAsTikz(plt, file.path(plotDirPath, sprintf("L96_n17_best_plot.tex")), width=8, heigh=4)

