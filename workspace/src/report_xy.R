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

filePaths <- list.files(evalDirPath, pattern="^solverError_eval_.*\\.json", full.names=TRUE)
solverError <-
  lapply(filePaths, \(filePath) {
    info <- readInfo(filePath = filePath)
    tibble(
      system = info$systemLabel,
      methodPrecision = info$solverPrecision,
      dataPrecision = str_sub(info$truthName, start=-1, end=-1),
      systemPrecision = str_sub(info$truthName, start=-2, end=-2),
      mean = info$vpt$mean,
      n = info$vpt$n,
      ci95Lower = info$vpt$confInt95[1],
      ci95Upper = info$vpt$confInt95[2],
      nObs = 1,
      nDegs = NA,
      nStep = NA,
      type = "Solver"
    )
  }) |>
  bind_rows()




extremePrecision <-
  bind_rows(
    errorStats |>
      filter(
        system == "L63",
        systemPrecision %in% c("m", "y", "x"),
        dataPrecision == "d",
        testMode == "sequential",
        methodPrecision == "m",
        normalization == "none"
      ) |>
      select(
        system,
        systemPrecision,
        dataPrecision,
        methodPrecision,
        nObs,
        nSteps,
        nDegs,
        mean,
        n,
        ci95Lower,
        ci95Upper
      ) |>
      mutate(type = "PolyProp") |>
      filter(nDegs == 15, nSteps == 2^5, nObs == 2^15),
    solverError |>
      filter(
        systemPrecision %in% c("m", "x", "y"),
        system == "L63",
        methodPrecision %in% c("m", "x", "y"),
        dataPrecision == "d"
    )
  )


#TODO

tableData <-
  extremePrecision |>
  mutate(
    group = case_when(
      systemPrecision %in% c("x", "y") & methodPrecision %in% c("x", "y") & type == "Solver" ~ "Exterme Precision",
      methodPrecision == "m" & type == "Solver" ~ "High Precision",
      type == "PolyProp" ~ "PolyProp"
    ),
    System = case_match(systemPrecision, "m" ~ "RK4-m", "y" ~ "RK4-y", "x" ~ "TI-x"),
    Method = case_when(
      type == "PolyProp" ~ "PolyProp",
      methodPrecision == "m" ~ "RK4-m",
      methodPrecision == "x" ~ "TI-x",
      methodPrecision == "y" ~ "RK4-y"
    ),
    Data = "64 bit",
    ci = sprintf("[%.2f, %.2f]", ci95Lower, ci95Upper),
    reps = sprintf("$10^{%d}$", log10(n))
  ) |>
  arrange(group, systemPrecision) |>
  select(group, System, Data, Method, mean, ci, reps)

tbl <-
  tableData |>
  mutate(reps = I(reps)) |>
  group_by(group) |>
  gt() |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    mean = "VPT",
    ci = "95% CI",
    reps = "Reps."
  ) |>
  tab_spanner(
    "Precision",
    ends_with("Precision")
  ) |>
  fmt_number(columns = mean, decimals = 2)

writeTableAsLatex(tbl, file.path(tableDirPath, "L63_xprec_table.tex"))






