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
errorStatsFilePath <- file.path(evalDirPath, sprintf("errorStats_%s.csv", errorMetric))
errorStats <- read_csv(errorStatsFilePath)
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
    .by = c(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs)
  ) |>
  left_join(
    errorStats, join_by(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs, deg == nDegs, step == nSteps)
  )

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
solverVptL63Low <- solverError |> filter(system == "L63", truthPrecision  == "md", solverPrecision == "s") |> pull(vpt)
solverVptL63Low2 <- solverError |> filter(system == "L63", truthPrecision  == "ms", solverPrecision == "d") |> pull(vpt)
solverVptL63Medi <- solverError |> filter(system == "L63", truthPrecision  == "md", solverPrecision == "d") |> pull(vpt)
solverVptL63High <- solverError |> filter(system == "L63", truthPrecision  == "md", solverPrecision == "m") |> pull(vpt)
solverVptTCSALow <- solverError |> filter(system == "TCSA", truthPrecision  == "md", solverPrecision == "s") |> pull(vpt)
solverVptTCSAMedi <- solverError |> filter(system == "TCSA", truthPrecision  == "md", solverPrecision == "d") |> pull(vpt)
solverVptTCSAHigh <- solverError |> filter(system == "TCSA", truthPrecision  == "md", solverPrecision == "m") |> pull(vpt)



# statistically indistinguishable from solver
# L63 sdd
bbest |>
  filter(systemPrecision=="s", dataPrecision=="d", methodPrecision=="d", system == "L63") |>
  mutate(rk4 = solverVptL63Low) |>
  filter(ci95Upper > rk4)
# L63 dsd
bbest |>
  filter(systemPrecision=="d", dataPrecision=="s", methodPrecision=="d", system == "L63") |>
  mutate(rk4 = solverVptL63Low2) |>
  filter(ci95Upper > rk4)
# L63 ddm
bbest |>
  filter(systemPrecision=="d", dataPrecision=="d", methodPrecision=="m", system == "L63") |>
  mutate(rk4 = solverVptL63Medi) |>
  filter(ci95Upper > rk4)
# L63 mdm
bbest |>
  filter(systemPrecision=="m", dataPrecision=="d", methodPrecision=="m", system == "L63", testMode == "sequential") |>
  mutate(rk4 = solverVptL63High) |>
  filter(ci95Upper > rk4)

# significantly better than solver
# L63 mdm
bbest |>
  filter(systemPrecision=="m", dataPrecision=="d", methodPrecision=="m", system == "L63", testMode == "sequential") |>
  mutate(rk4 = solverVptL63High) |>
  filter(ci95Lower > rk4)






pltData <-
  bbest |>
  filter(system == "L63", testMode == "sequential", methodPrecision %in% c("s", "d"), dataPrecision == "d", systemPrecision == "d") |>
  mutate(colorLabel = sprintf("%s %s", methodPrecision, normalization)) |>
  #mutate(colorLabel = methodPrecision) |>
  mutate(label = sprintf("$%d$, $2^{%s}$", deg, log2(defaults$L63$timeStep * step)))

plt <-
  pltData |>
  ggplot(aes(x = nObs, y = mean, colour = colorLabel)) +
  geom_line(aes(linetype = normalization)) + geom_point(aes(shape = normalization)) +
    scale_x_log10() +
    xlab("$n$") + ylab(errorMetricLabelText) +
    labs(color = "label") +
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
saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_d_normalization_bbest_plot.tex"), width=8, heigh=4)






precisionLabelEquivalentClass <- c(
  "sds" = "[sdm]ds",
  "sdd" = "sd[dm]",
  "sdm" = "sd[dm]",
  "dds" = "[sdm]ds",
  "ddd" = "[dm]dd",
  "dsd" = "[dm]sd",
  "msd" = "[dm]sd",
  "ddm" = "ddm",
  "mds" = "[sdm]ds",
  "mdd" = "[dm]dd",
  "mdm" = "mdm"
)
pltData <-
  bbest |>
  filter(system == "L63", testMode == "sequential", dataPrecision %in% c("s", "d"), normalization == "full" | methodPrecision == "m") |>
  mutate(precisionLabel = sprintf("%s%s%s", systemPrecision, dataPrecision, methodPrecision)) |>
  mutate(precisionLabelEqui = precisionLabelEquivalentClass[precisionLabel]) |>
  filter(!precisionLabel %in% c("dds", "ddd", "sds", "sdm", "dsd"))

plt <-
  pltData |>
  ggplot(aes(x = nObs, y = mean, colour = precisionLabelEqui)) +
  geom_line() + geom_point() +
    scale_x_log10() +
    xlab("$n$") + ylab(errorMetricLabelText) +
    labs(color = "Precision") +
    geom_hline(yintercept=solverVptL63Low, col="#777") +
    geom_hline(yintercept=solverVptL63Low2, col="#777") +
    geom_hline(yintercept=solverVptL63Medi, col="#777") +
    geom_hline(yintercept=solverVptL63High, col="#777") +
    annotate("text", hjust = "left", label = "RK4 ODE solver [dm]sd", x = 8, y = 16.6, col="#777") +
    annotate("text", hjust = "left", label = "RK4 ODE solver [dm]ds/sd[dm]", x = 8, y = 13.0, col="#777") +
    annotate("text", hjust = "left", label = "RK4 ODE solver mdd/ddm", x = 8, y = 31.2, col="#777") +
    annotate("text", hjust = "left", label = "RK4 ODE solver mdm", x = 8, y = 35.6, col="#777") +
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
set.seed(6)
saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_prec_bbest_plot.tex"), width=8, heigh=4)







pltData <-
  bbest |>
  filter(system == "L63", systemPrecision == "m", dataPrecision == "d", methodPrecision == "m", normalization == "none") |>
  mutate(colorLabel = testMode) |>
  mutate(label = sprintf("$%d$, $2^{%s}$", deg, log2(defaults$L63$timeStep * step)))

plt <-
  pltData |>
  ggplot(aes(x = nObs, y = mean, colour = colorLabel)) +
  geom_line() + geom_point() +
    scale_x_log10() +
    xlab("$n$") + ylab(errorMetricLabelText) +
    labs(color = "Test Mode") +
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
saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_mdm_RandVsSeq_bbest_plot.tex"), width=8, heigh=4)






experiErrorStats <-
  errorStats |>
  filter(
    system == "L63",
    systemPrecision == "m",
    dataPrecision == "d",
    testMode == "sequential",
    methodPrecision == "m",
    normalization == "none",
  )

best <-
  experiErrorStats |>
  summarise(
    deg = nDegs[which.max(mean)],
    .by = c(nObs, nSteps)
  ) |>
  left_join(
    experiErrorStats, join_by(nObs, nSteps, deg == nDegs)
  )

plt <-
  best |>
  mutate(deltaT = factor(sprintf("$2^{%d}$", log2(defaults$L63$timeStep*nSteps))) |> fct_reorder(nSteps)) |>
  ggplot(aes(x = nObs, y = mean, color = deltaT)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  scale_color_manual(values = colors8) +
  xlab("$n$") + ylab(errorMetricLabelText) +
  labs(color = "$\\Delta\\!t$") +
  geom_hline(yintercept=solverVptL63Low, col="#777") +
  geom_hline(yintercept=solverVptL63Medi, col="#777") +
  geom_hline(yintercept=solverVptL63High, col="#777") +
  annotate("text", hjust = "left", label = "RK4 ODE solver 32bit", x = 8, y = 13.0, col="#777") +
  annotate("text", hjust = "left", label = "RK4 ODE solver 64bit", x = 8, y = 31.2, col="#777") +
  annotate("text", hjust = "left", label = "RK4 ODE solver 512bit", x = 8, y = 35.6, col="#777") +
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
saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_mdm_n_s_VPT_best_marked_plot.tex"), width=8, heigh=4)






#
#
#
# experiErrorStats <-
#   errorStats |>
#   filter(
#     system == "L63",
#     systemPrecision == "m",
#     dataPrecision == "d",
#     testMode == "sequential",
#     methodPrecision == "m",
#     normalization == "none",
#   )
#
# best <-
#   experiErrorStats |>
#   summarise(
#     deg = nDegs[which.max(mean)],
#     .by = c(nObs, nSteps)
#   ) |>
#   left_join(
#     experiErrorStats, join_by(nObs, nSteps, deg == nDegs)
#   )
# bbest_L63_mdm <-
#   bbest |>
#   filter(
#     system == "L63",
#     systemPrecision == "m",
#     dataPrecision == "d",
#     testMode == "sequential",
#     methodPrecision == "m",
#     normalization == "none",
#   )
# set.seed(1)
# plt <-
#   best |>
#   ggplot(aes(x = nObs, y = nSteps, fill = mean)) +
#   geom_raster() +
#   geom_line(data = bbest_L63_mdm, aes(y = step), color="red") +
#   geom_point(data = bbest_L63_mdm, aes(y = step), color="red") +
#   geom_label(
#     data = bbest_L63_mdm,
#     aes(y = step, label = deg),
#     color = "red",
#     label.padding = unit(0.1, "lines"),
#     label.r = unit(0.0, "lines"),
#     label.size = unit(0.2, "lines"),
#   ) +
#   scale_fill_viridis_c(limits = c(0, NA)) +
#   scale_x_log10() +
#   scale_y_log10(breaks = 2^(0:7), labels = sprintf("$2^{%d}$", log2(defaults$L63$timeStep*2^(0:7)))) +
#   xlab("$n$") + ylab("$\\Delta\\!t$") + labs(fill = "VPT")
# set.seed(1)
# saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_mdm_n_s_VPT_best_timeStep_plot.tex"), width=5.5, heigh=3)
#







experiErrorStats <-
  errorStats |>
  filter(
    system == "L63",
    systemPrecision == "m",
    dataPrecision == "m",
    testMode == "sequential",
    methodPrecision == "m",
    normalization == "none",
  )

best <-
  experiErrorStats |>
  summarise(
    deg = nDegs[which.max(mean)],
    .by = c(nObs, nSteps)
  ) |>
  left_join(
    experiErrorStats, join_by(nObs, nSteps, deg == nDegs)
  )

plt <-
  best |>
  filter(nSteps > 1 | nObs <= 2^10) |>
  mutate(deltaT = factor(sprintf("$2^{%d}$", log2(defaults$L63$timeStep*nSteps))) |> fct_reorder(nSteps)) |>
  ggplot(aes(x = nObs, y = mean, color = deltaT)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  scale_color_manual(values = colors8) +
  xlab("$n$") + ylab(errorMetricLabelText) +
  labs(color = "$\\Delta\\!t$") +
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
saveGgplotAsTikz(plt, file.path(plotDirPath, "L63_mmm_n_s_VPT_best_cut_plot.tex"), width=8, heigh=4)










experiErrorStats <-
  errorStats |>
  filter(
    system == "TCSA",
    systemPrecision == "m",
    dataPrecision == "d",
    testMode == "sequential",
    methodPrecision == "m",
    normalization == "none",
  )

best <-
  experiErrorStats |>
  summarise(
    deg = nDegs[which.max(mean)],
    .by = c(nObs, nSteps)
  ) |>
  left_join(
    experiErrorStats, join_by(nObs, nSteps, deg == nDegs)
  )

plt <-
  best |>
  mutate(deltaT = factor(sprintf("$2^{%d}$", log2(defaults$TCSA$timeStep*nSteps))) |> fct_reorder(nSteps)) |>
  ggplot(aes(x = nObs, y = mean, color = deltaT)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  scale_color_manual(values = colors8) +
  xlab("$n$") + ylab(errorMetricLabelText) +
  labs(color = "$\\Delta\\!t$") +
  geom_hline(yintercept=solverVptTCSALow, col="#777") +
  geom_hline(yintercept=solverVptTCSAMedi, col="#777") +
  geom_hline(yintercept=solverVptTCSAHigh, col="#777") +
  annotate("text", hjust = "left", label = "RK4 ODE solver 32bit", x = 8, y = 10.5, col="#777") +
  annotate("text", hjust = "left", label = "RK4 ODE solver 64bit", x = 8, y = 31.3, col="#777") +
  annotate("text", hjust = "left", label = "RK4 ODE solver 512bit", x = 8, y = 36.5, col="#777") +
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
saveGgplotAsTikz(plt, file.path(plotDirPath, "TCSA_mdm_n_s_VPT_best_marked_plot.tex"), width=8, heigh=4)







solverVptL96dsd <-
  solverError |>
  filter(
    startsWith(system, "L96"),
    solverPrecision == "d",
    truthPrecision == "ds"
  )

pltData <-
  bbest |>
  filter(
    startsWith(system, "L96"),
    testMode == "sequential",
    methodPrecision == "d",
    dataPrecision == "s",
    systemPrecision == "d"
  ) |>
  mutate(colorLabel = system)


plt <-
  pltData |>
  ggplot(aes(x = nObs, y = mean, color = colorLabel)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  #scale_color_manual(values = colors8) +
  xlab("$n$") + ylab(errorMetricLabelText) +
  labs(color = "System") +
  #geom_hline(yintercept = solverVptL96dsd |> reframe(vpt = range(vpt)) |> pull(vpt), col="#777") +
  #annotate("text", hjust = "left", label = "RK4 ODE solver 64bit range", x = 8, y = 17, col="#777") +
  geom_hline(data = solverVptL96dsd, mapping = aes(yintercept = vpt, color = system)) +
  annotate("text", hjust = "left", label = "RK4 ODE solver 64bit", x = 8, y = 15.8, col="#777") +
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
saveGgplotAsTikz(plt, file.path(plotDirPath, "L96_dsd_f_s_VPT_bbest_marked_plot.tex"), width=8, heigh=4)






solverVptL96sdd <-
  solverError |>
  filter(
    startsWith(system, "L96"),
    solverPrecision == "d",
    truthPrecision == "sd"
  )

pltData <-
  bbest |>
  filter(
    startsWith(system, "L96"),
    testMode == "sequential",
    methodPrecision == "d",
    dataPrecision == "d",
    systemPrecision == "s"
  ) |>
  mutate(colorLabel = system)


plt <-
  pltData |>
  ggplot(aes(x = nObs, y = mean, color = colorLabel)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  xlab("$n$") + ylab(errorMetricLabelText) +
  labs(color = "System") +
  geom_hline(data = solverVptL96sdd, mapping = aes(yintercept = vpt, color = system)) +
  annotate("text", hjust = "left", label = "RK4 ODE solver 64bit", x = 8, y = 15.8, col="#777") +
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
saveGgplotAsTikz(plt, file.path(plotDirPath, "L96_sdd_f_s_VPT_bbest_marked_plot.tex"), width=8, heigh=4)


