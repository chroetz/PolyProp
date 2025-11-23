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


tableData <-
  bestResults |>
  mutate(
    systemPrecision = precisionToBit[systemPrecision],
    dataPrecision = precisionToBit[dataPrecision],
    methodPrecision = precisionToBit[methodPrecision]
  )  |>
  mutate(timeStep = sapply(defaults[system], \(x) x$timeStep)) |>
  mutate(
    nObsText = I(sprintf("$2^{%d} = %d$", log2(nObs), nObs)),
    stepText = I(sprintf("$2^{%d}$", log2(step*timeStep)))
  ) |>
  select(systemPrecision, dataPrecision, methodPrecision, nObsText, stepText, deg, mean)

tbl <-
  tableData |>
  gt() |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    systemPrecision = "system",
    dataPrecision = "data",
    methodPrecision = "method",
    nObsText = I("$n$"),
    stepText = I("$\\stepsize$"),
    deg = "Degree",
    mean = "VPT"
  ) |>
  tab_spanner(
    "Precision",
    ends_with("Precision")
  ) |>
  fmt_number(columns = mean, decimals = 1) |>
  cols_align(columns=ends_with("Precision"), align="right") |>
  cols_align(columns=nObsText, align="left")

writeTableAsLatex(tbl, file.path(tableDirPath, "L63_bbbest_table.tex"))










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

tableData <-
  solverError |>
  separate_wider_position(truthPrecision, widths=c(systemPrecision = 1,dataPrecision = 1)) |>
  rename(methodPrecision = solverPrecision) |>
  mutate(timeStep = sapply(defaults[system], \(x) x$timeStep)) |>
  mutate(stepText = I(sprintf("$2^{%d}$", log2(timeStep)))) |>
  mutate(
    systemPrecision = precisionToBit[systemPrecision],
    dataPrecision = precisionToBit[dataPrecision],
    methodPrecision = precisionToBit[methodPrecision]
  ) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, stepText, vpt)

tbl <-
  tableData |>
  group_by(system) |>
  arrange(system, vpt) |>
  gt() |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    #system = "System",
    systemPrecision = "system",
    dataPrecision = "data",
    methodPrecision = "method",
    stepText = I("$\\stepsize_0$"),
    vpt = "VPT"
  ) |>
  tab_spanner(
    "Precision",
    ends_with("Precision")
  ) |>
  fmt_number(columns=vpt, decimals=1) |>
  cols_align(columns=ends_with("Precision"), align="right")

writeTableAsLatex(tbl, file.path(tableDirPath, "Solver_VPT_table.tex"))







bbest <-
  errorStats |>
  filter(system == "TCSA" | nSteps != 1) |> # exclude estimation at the solver time step for L63 and L96 as this is polynomial of degree 8, so that polynomial propagator regression might have an unfair advantage in this case
  summarise(
    deg = nDegs[which.max(mean)],
    step = nSteps[which.max(mean)],
    .by = c(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs)
  ) |>
  left_join(
    errorStats, join_by(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs, deg == nDegs, step == nSteps)
  )

solverVptL96 <-
  solverError |>
  filter(startsWith(system, "L96"))

bbestL96 <-
  bbest |>
  filter(
    startsWith(system, "L96"),
    testMode == "sequential",
    methodPrecision == "d",
  ) |>
  mutate(truthPrecision = paste0(systemPrecision, dataPrecision)) |>
  left_join(solverVptL96 |> rename(solverVpt = vpt), join_by(truthPrecision, system))



bbbestL96 <-
  bbestL96 |>
  filter(mean == max(mean), .by = c(system, truthPrecision, methodPrecision, testMode)) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, nObs, deg, step, mean, sd, n, ci95Lower, ci95Upper, solverVpt)

tbl <-
  bbbestL96 |>
  mutate(
    ci95 = sprintf("[%.1f, %.1f]", ci95Lower, ci95Upper),
    systemPrecision = precisionToBit[systemPrecision],
    dataPrecision = precisionToBit[dataPrecision],
    methodPrecision = precisionToBit[methodPrecision],
    nObsText = I(sprintf("$2^{%d}$", log2(nObs))),
    stepText = I(sprintf("$2^{%d}$", log2(step*defaults$L96$timeStep)))
  ) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, nObsText, deg, stepText, mean, ci95, solverVpt) |>
  arrange(systemPrecision, system) |>
  gt() |>
  opt_row_striping(row_striping = TRUE) |>
  opt_vertical_padding(scale = 0) |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    system = "System",
    systemPrecision = "system",
    dataPrecision = "data",
    methodPrecision = "method",
    nObsText = I("$n$"),
    deg = I("$p$"),
    stepText = I("$\\stepsize$"),
    mean = "VPT",
    ci95 = "95%-CI",
    solverVpt = "VPT"
  ) |>
  tab_spanner(
    "Precision",
    columns = ends_with("Precision")
  ) |>
  tab_spanner(
    "Polynomial Prop.",
    columns = c(mean, ci95)
  ) |>
  tab_spanner(
    "RK4",
    columns = c(solverVpt)
  ) |>
  fmt_number(
    c(mean, solverVpt),
    decimals = 1
  ) |>
  cols_align(
    c(ends_with("Precision"), nObsText, deg, stepText, ci95),
    align="center"
  )
writeTableAsLatex(tbl, file.path(tableDirPath, "L96_bbbest_table.tex"))





betterThanSolver <-
  bbestL96 |>
  filter(ci95Upper >= solverVpt) |>
  filter(nObs == min(nObs), .by = c(system, truthPrecision, methodPrecision, testMode)) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, nObs, deg, step, mean, sd, n, ci95Lower, ci95Upper, solverVpt)

tbl <-
  betterThanSolver |>
  mutate(
    ci95 = sprintf("[%.1f, %.1f]", ci95Lower, ci95Upper),
    systemPrecision = precisionToBit[systemPrecision],
    dataPrecision = precisionToBit[dataPrecision],
    methodPrecision = precisionToBit[methodPrecision],
    nObsText = I(sprintf("$2^{%d}$", log2(nObs))),
    stepText = I(sprintf("$2^{%d}$", log2(step*defaults$L96$timeStep)))
  ) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, nObsText, deg, stepText, mean, ci95, solverVpt) |>
  arrange(systemPrecision, system) |>
  gt() |>
  opt_row_striping(row_striping = TRUE) |>
  opt_vertical_padding(scale = 0) |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    system = "System",
    systemPrecision = "system",
    dataPrecision = "data",
    methodPrecision = "method",
    nObsText = I("$n$"),
    deg = I("$p$"),
    stepText = I("$\\stepsize$"),
    mean = "VPT",
    ci95 = "95%-CI",
    solverVpt = "VPT"
  ) |>
  tab_spanner(
    "Precision",
    columns = ends_with("Precision")
  ) |>
  tab_spanner(
    "Polynomial Prop.",
    columns = c(mean, ci95)
  ) |>
  tab_spanner(
    "RK4",
    columns = c(solverVpt)
  ) |>
  fmt_number(
    c(mean, solverVpt),
    decimals = 1
  ) |>
  cols_align(
    c(ends_with("Precision"), nObsText, deg, stepText, ci95),
    align="center"
  )
writeTableAsLatex(tbl, file.path(tableDirPath, "L96_likeSolver_table.tex"))






fixedSetting <-
  errorStats |>
  filter(startsWith(system, "L96"), nObs == 2^17, nSteps == 2^3) |>
  filter(mean == max(mean), .by = c(system, systemPrecision, dataPrecision)) |>
  mutate(truthPrecision = paste0(systemPrecision, dataPrecision)) |>
  left_join(solverVptL96 |> rename(solverVpt = vpt), join_by(truthPrecision, system)) |>
  rename(deg = nDegs, step = nSteps) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, nObs, deg, step, mean, sd, n, ci95Lower, ci95Upper, solverVpt)

tbl <-
  fixedSetting |>
  mutate(
    ci95 = sprintf("[%.1f, %.1f]", ci95Lower, ci95Upper),
    systemPrecision = precisionToBit[systemPrecision],
    dataPrecision = precisionToBit[dataPrecision],
    methodPrecision = precisionToBit[methodPrecision],
    nObsText = I(sprintf("$2^{%d}$", log2(nObs))),
    stepText = I(sprintf("$2^{%d}$", log2(step*defaults$L96$timeStep)))
  ) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, nObsText, deg, stepText, mean, ci95, solverVpt) |>
  arrange(systemPrecision, system) |>
  gt() |>
  opt_row_striping(row_striping = TRUE) |>
  opt_vertical_padding(scale = 0) |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    system = "System",
    systemPrecision = "system",
    dataPrecision = "data",
    methodPrecision = "method",
    nObsText = I("$n$"),
    deg = I("$p$"),
    stepText = I("$\\stepsize$"),
    mean = "VPT",
    ci95 = "95%-CI",
    solverVpt = "VPT"
  ) |>
  tab_spanner(
    "Precision",
    columns = ends_with("Precision")
  ) |>
  tab_spanner(
    "Polynomial Prop.",
    columns = c(mean, ci95)
  ) |>
  tab_spanner(
    "RK4",
    columns = c(solverVpt)
  ) |>
  fmt_number(
    c(mean, solverVpt),
    decimals = 1
  ) |>
  cols_align(
    c(ends_with("Precision"), nObsText, deg, stepText, ci95),
    align="center"
  )
writeTableAsLatex(tbl, file.path(tableDirPath, "L96_fixed_table.tex"))






fixed6Setting <-
  errorStats |>
  filter(startsWith(system, "L96"), nObs == 2^17, nSteps == 2^3, nDegs == 6) |>
  mutate(truthPrecision = paste0(systemPrecision, dataPrecision)) |>
  left_join(solverVptL96 |> rename(solverVpt = vpt), join_by(truthPrecision, system)) |>
  rename(deg = nDegs, step = nSteps) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, nObs, deg, step, mean, sd, n, ci95Lower, ci95Upper, solverVpt)

tbl <-
  fixed6Setting |>
  mutate(
    ci95 = sprintf("[%.1f, %.1f]", ci95Lower, ci95Upper),
    systemPrecision = precisionToBit[systemPrecision],
    dataPrecision = precisionToBit[dataPrecision],
    methodPrecision = precisionToBit[methodPrecision],
    nObsText = I(sprintf("$2^{%d}$", log2(nObs))),
    stepText = I(sprintf("$2^{%d}$", log2(step*defaults$L96$timeStep)))
  ) |>
  select(system, systemPrecision, dataPrecision, methodPrecision, nObsText, deg, stepText, mean, ci95, solverVpt) |>
  arrange(systemPrecision, system) |>
  gt() |>
  opt_row_striping(row_striping = TRUE) |>
  opt_vertical_padding(scale = 0) |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    system = "System",
    systemPrecision = "system",
    dataPrecision = "data",
    methodPrecision = "method",
    nObsText = I("$n$"),
    deg = I("$p$"),
    stepText = I("$\\stepsize$"),
    mean = "VPT",
    ci95 = "95%-CI",
    solverVpt = "VPT"
  ) |>
  tab_spanner(
    "Precision",
    columns = ends_with("Precision")
  ) |>
  tab_spanner(
    "Polynomial Prop.",
    columns = c(mean, ci95)
  ) |>
  tab_spanner(
    "RK4",
    columns = c(solverVpt)
  ) |>
  fmt_number(
    c(mean, solverVpt),
    decimals = 1
  ) |>
  cols_align(
    c(ends_with("Precision"), nObsText, deg, stepText, ci95),
    align="center"
  )
writeTableAsLatex(tbl, file.path(tableDirPath, "L96_fixed6_table.tex"))
