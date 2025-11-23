source("common_run.R")
source("common_defaults.R")

default <- getDefaults()

args <- commandArgs(TRUE)
if (length(args) == 0) {
  runMode <- "s"
  randomSeeds <- "1"
} else {
  stopifnot(length(args)==2)
  runMode <- args[1]
  randomSeeds <- args[2]
}

cmds <- c(
  # sd X x s with different method precision X and default normalization x
  sprintf("Rscript run_forecast_main.R L63 sdmns 1:8 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 sdmns 9:12 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 sdmns 13:14 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 sdmns 15 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 sdmns 16 %s %s", runMode, randomSeeds),

  # sd X x s with different method precision X and default normalization x
  sprintf("Rscript run_forecast_main.R L63 mdmns 1:8 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 mdmns 9:12 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 mdmns 13:14 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 mdmns 15 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 mdmns 16 %s %s", runMode, randomSeeds),

  # main specification on 64 bit solutions
  sprintf("Rscript run_forecast_main.R L63 ddmns 1:8 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ddmns 9:12 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ddmns 13:14 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ddmns 15 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ddmns 16 %s %s", runMode, randomSeeds),

  # main specification with random start state
  sprintf("Rscript run_forecast_main.R L63 mdmnr 1:8 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 mdmnr 9:12 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 mdmnr 13:14 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 mdmnr 15 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 mdmnr 16 %s %s", runMode, randomSeeds),

  # main specification on TCSA
  sprintf("Rscript run_forecast_main.R TCSA mdmns 1:8 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R TCSA mdmns 9:12 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R TCSA mdmns 13:14 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R TCSA mdmns 15 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R TCSA mdmns 16 %s %s", runMode, randomSeeds),

  NULL
)

runCmdsLocal(cmds, parallel=FALSE)
