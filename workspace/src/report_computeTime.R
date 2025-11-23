options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")


dataSummary <- read_csv(file.path(evalDirPath, "computeTime.csv"))


dataL63 <-
  dataSummary |>
  filter(system == "L63") |>
  filter(label != "mmm") |>
  separate_wider_position(label, c(systemPrecision = 1, dataPrecision = 1, methodPrecision = 1, normalization = 1, testMode = 1))

dataL63Summary <-
  dataL63|>
  summarise(
    meanTime = sum(meanTime*nReps)/sum(nReps),
    nReps = sum(nReps),
    .by = c(methodPrecision, n, nDeg)
  )


dataTCSA <-
  dataSummary |>
  filter(system == "TCSA")

dataTCSASummary <-
  dataTCSA|>
  summarise(
    meanTime = sum(meanTime*nReps)/sum(nReps),
    nReps = sum(nReps),
    .by = c(n, nDeg)
  ) |>
  arrange(n, nDeg)


dataBothSummary <-
  bind_rows(
    dataL63Summary |>
      filter(methodPrecision == "m") |>
      select(-methodPrecision),
    dataTCSASummary
  ) |>
  summarise(
    meanTime = sum(meanTime*nReps)/sum(nReps),
    nReps = sum(nReps),
    .by = c(n, nDeg)
  )


plt <-
  dataBothSummary |>
  mutate(nText = sprintf("$2^{%d}$", log2(n))) |>
  mutate(nText = factor(nText) |> fct_reorder(n)) |>
  ggplot(aes(x = nDeg, y = meanTime, color = nText)) +
  geom_point() + geom_line() +
  scale_y_continuous(
    name = "Compute Time",
    transform = "log10",
    breaks = c (0.1, 1, 10, 60, 600, 3600, 36000, 3600*60),
    labels = c("100ms", "1s", "10s", "1m", "10m", "1h", "10h", "60h"),
    minor_breaks = NULL
  ) +
  labs(color = "$n$", x = "Degree $p$")
saveGgplotAsTikz(plt, "../img/compute_time_vs_deg_m.tex", width=8, height=4)

plt <-
  dataL63Summary |>
  filter(methodPrecision == "d") |>
  mutate(nText = sprintf("$2^{%d}$", log2(n))) |>
  mutate(nText = factor(nText) |> fct_reorder(n)) |>
  ggplot(aes(x = nDeg, y = meanTime, color = nText)) +
  geom_point() + geom_line() +
  scale_y_continuous(
    name = "Compute Time",
    transform = "log10",
    breaks = c (0.003, 0.01, 0.03, 0.1, 0.3, 1),
    labels = c("3ms", "10ms", "30ms", "100ms", "300ms", "1s"),
    minor_breaks = NULL
  ) +
  labs(color = "$n$", x = "Degree $p$")
saveGgplotAsTikz(plt, "../img/compute_time_vs_deg_d.tex", width=8, height=4)

plt <-
  dataL63Summary |>
  filter(methodPrecision == "s") |>
  mutate(nText = sprintf("$2^{%d}$", log2(n))) |>
  mutate(nText = factor(nText) |> fct_reorder(n)) |>
  ggplot(aes(x = nDeg, y = meanTime, color = nText)) +
  geom_point() + geom_line() +
  scale_y_continuous(
    name = "Compute Time",
    transform = "log10",
    breaks = c (0.003, 0.01, 0.03, 0.1, 0.3, 1),
    labels = c("3ms", "10ms", "30ms", "100ms", "300ms", "1s"),
    minor_breaks = NULL
  ) +
  labs(color = "$n$", x = "Degree $p$")
saveGgplotAsTikz(plt, "../img/compute_time_vs_deg_s.tex", width=8, height=4)





dataL96 <-
  dataSummary |>
  filter(startsWith(system, "L96")) |>
  separate_wider_delim(system, "D", names=c("system", "dimension")) |>
  mutate(dimension = as.integer(dimension))

dataL96Summary <-
  dataL96|>
  summarise(
    meanTime = sum(meanTime*nReps)/sum(nReps),
    nReps = sum(nReps),
    .by = c(n, nDeg, dimension)
  ) |>
  arrange(n, nDeg, dimension)

plt <-
  dataL96Summary |>
  filter(n %in% c(2^11, 2^14, 2^17), nDeg %in% c(2,5,8)) |>
  mutate(nText = sprintf("$2^{%d}$", log2(n))) |>
  mutate(nText = factor(nText) |> fct_reorder(n)) |>
  #mutate(dimension = factor(dimension)) |>
  mutate(nDeg = factor(nDeg)) |>
  ggplot(aes(x = dimension, y = meanTime, color = nDeg, shape = nText, linetype = nText)) +
  geom_point() + geom_line() +
  scale_y_continuous(
    name = "Compute Time",
    transform = "log10",
    breaks = c (0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 60, 180),
    labels = c("3ms", "10ms", "30ms", "100ms", "300ms", "1s", "3s", "10s", "1min", "3min"),
    minor_breaks = NULL
  ) +
  labs(color = "Degree $p$", x = "Dimension $d$", shape = "$n$", linetype = "$n$")
saveGgplotAsTikz(plt, "../img/compute_L96_time_vs_dim_d.tex", width=8, height=4)


plt <-
  dataL96Summary |>
  filter(n %in% c(2^11, 2^14, 2^17), dimension %in% c(5, 7, 9)) |>
  mutate(nText = sprintf("$2^{%d}$", log2(n))) |>
  mutate(nText = factor(nText) |> fct_reorder(n)) |>
  mutate(dimension = factor(dimension)) |>
  #mutate(nDeg = factor(nDeg)) |>
  ggplot(aes(x = nDeg, y = meanTime, color = dimension, shape = nText, linetype = nText)) +
  geom_point() + geom_line() +
  scale_y_continuous(
    name = "Compute Time",
    transform = "log10",
    breaks = c (0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 60, 180),
    labels = c("3ms", "10ms", "30ms", "100ms", "300ms", "1s", "3s", "10s", "1min", "3min"),
    minor_breaks = NULL
  ) +
  labs(color = "Dimension $d$", x = "Degree $p$", shape = "$n$", linetype = "$n$")
saveGgplotAsTikz(plt, "../img/compute_L96_time_vs_deg_d.tex", width=8, height=4)












## Time to VPT
# errorMetric <- "vpt0.5"
# errorStatsFilePath <- file.path(evalDirPath, sprintf("errorStats_%s.csv", errorMetric))
# errorStats <- read_csv(errorStatsFilePath)
#
# filePaths <- list.files(evalDirPath, pattern="^solverError_eval_.*\\.json", full.names=TRUE)
# solverError <-
#   lapply(filePaths, \(filePath) {
#     info <- readInfo(filePath = filePath)
#     tibble(
#       system = info$systemLabel,
#       solverPrecision = info$solverPrecision,
#       truthPrecision = str_sub(info$truthName, start=-2, end=-1),
#       vpt = info$vpt$mean
#     )
#   }) |>
#   bind_rows()
#
# bbest <-
#   errorStats |>
#   filter(system == "TCSA" | nSteps != 1) |> # exclude estimation at the solver time step for L63 and L96 as this is polynomial of degree 8, so that polynomial propagator regression might have an unfair advantage in this case
#   summarise(
#     deg = nDegs[which.max(mean)],
#     step = nSteps[which.max(mean)],
#     .by = c(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs)
#   ) |>
#   left_join(
#     errorStats, join_by(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs, deg == nDegs, step == nSteps)
#   )
#
# solverVptL96 <-
#   solverError |>
#   filter(startsWith(system, "L96"))
#
# bbestL96 <-
#   bbest |>
#   filter(
#     startsWith(system, "L96"),
#     testMode == "sequential",
#     methodPrecision == "d",
#   ) |>
#   mutate(truthPrecision = paste0(systemPrecision, dataPrecision)) |>
#   left_join(solverVptL96 |> rename(solverVpt = vpt), join_by(truthPrecision, system))
#
# dataL96Summary |>
#   semi_join(
#     bbestL96 |>
#       filter(mean >= 0.5*solverVpt) |>
#       separate_wider_delim(system, "D", names=c("system", "dimension")) |>
#       mutate(dimension = as.integer(dimension)) |>
#       select(dimension, nObs, deg),
#     join_by(dimension, n == nObs, nDeg == deg)
#   ) |>
#   filter(meanTime == min(meanTime), .by = c(dimension)) |>
#   arrange(dimension)

