options(tidyverse.quiet = TRUE)
library(tidyverse)
library(gt)
source("common_paths.R")
source("common_io.R")
source("common_defaults.R")



# default parameters
errorMetric <- "vpt0.5"
errorMetricLabel <- "VPT"
errorMetricLabelText <- "Valid Prediction Time [Lyapunov times]"
defaults <- getDefaults()

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived and const values
errorStatsFilePath <- file.path(evalDirPath, sprintf("errorStats_noisy_%s.csv", errorMetric))
errorStats <-
  read_csv(errorStatsFilePath) |>
  filter(
    system == "L63",
    testMode == "sequential",
    methodPrecision == "d",
    dataPrecision == "d",
    systemPrecision == "d",
    normalization == "full"
  )

colors8 <- c("#1F77E4", "#FF7F0E", "#2CA02C", "#D62728", "#BCBD00", "#920692", "#8A564B", "#FF77C2")

experiments <-
  errorStats |>
  select(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization) |>
  distinct()

bbest <-
  errorStats |>
  filter(system != "L63" | nSteps != 1) |> # exclude estimation at the solver time step for L63 as this is polynomial of degree 8, so that polynomial propagator regression might have an unfair advantage in this case
  summarise(
    deg = nDegs[which.max(mean)],
    step = nSteps[which.max(mean)],
    .by = c(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs, noiseLevels)
  ) |>
  left_join(
    errorStats, join_by(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs, deg == nDegs, step == nSteps, noiseLevels==noiseLevels)
  )

pltData <-
  bbest |>
  mutate(
    colorLabel = case_when(
      noiseLevels == 0 ~ "$0$",
      noiseLevels == 1 ~ "$1$",
      .default = sprintf("$10^{%d}$", as.integer(round(log10(noiseLevels))))
    ),
    colorLabel = fct_reorder(colorLabel, noiseLevels)
  ) |>
  mutate(label = sprintf("$%d$, $2^{%s}$", deg, log2(defaults$L63$timeStep * step)))

plt <-
  pltData |>
  ggplot(aes(x = nObs, y = mean, colour = colorLabel)) +
  geom_line() + geom_point() +
    scale_x_log10() +
    xlab("$n$") + ylab(errorMetricLabelText) +
    labs(color = "Noise Level") +
    ggrepel::geom_label_repel(
      aes(label = deg),
      min.segment.length=0,
      force_pull = 1,
      force = 20,
      max.overlaps  = Inf,
      box.padding = 0,
      label.padding = 0.1,
      point.padding = 0,
      label.r = 0.0,
      label.size = 0.2,
    )
set.seed(1)
saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_d_noisy_bbest_plot.tex"), width=8, heigh=4)



fixedStep <- 32
fixedN <- 1024

pltData <-
  errorStats |>
  filter(nSteps == fixedStep, nObs == fixedN) |>
  mutate(degree = as.factor(nDegs)) |>
  mutate(noiseLevels = ifelse(noiseLevels == 0, min(noiseLevels[noiseLevels>0]) / 100, noiseLevels))

allNoiseLevels <- sort(unique(errorStats$noiseLevels))
allNoiseLevelsTransformed <- sort(unique(pltData$noiseLevels))

plt <- pltData |>
  ggplot(aes(x=noiseLevels, y=mean, color=degree)) +
  geom_line() + geom_point() +
  scale_x_log10(
    breaks = allNoiseLevelsTransformed,
    labels = case_when(
      allNoiseLevels == 0 ~ "$0$",
      allNoiseLevels == 1 ~ "$1$",
      .default = sprintf("$10^{%d}$", as.integer(round(log10(allNoiseLevels))))
    )
  ) +
  ylim(c(0, NA)) +
  labs(
    x = "Noise Level",
    y = errorMetricLabelText,
    title = sprintf("$\\Delta\\!t = 2^{%d}$, $n = %d$", round(log2(2^(-10)*fixedStep)), fixedN)
  )
set.seed(1)
saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_d_noisy_fixStepAndN_plot.tex"), width=8, heigh=4)



fixedStep <- 32
fixedDeg <- 6

pltData <-
  errorStats |>
  filter(nSteps == fixedStep, nDegs == fixedDeg) |>
  mutate(
    colorLabel = case_when(
      noiseLevels == 0 ~ "$0$",
      noiseLevels == 1 ~ "$1$",
      .default = sprintf("$10^{%d}$", as.integer(round(log10(noiseLevels))))
    ),
    colorLabel = fct_reorder(colorLabel, noiseLevels)
  )

plt <- pltData |>
  ggplot(aes(x=nObs, y=mean, color=colorLabel)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  ylim(c(0, NA)) +
  labs(
    x = "$n$",
    y = errorMetricLabelText,
    color = "Noise Level",
    title = sprintf("$\\Delta\\!t = 2^{%d}$, degree $p = %d$", round(log2(2^(-10)*fixedStep)), fixedDeg)
  )
set.seed(1)
saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_d_noisy_fixStepAndDeg_plot.tex"), width=8, heigh=4)

