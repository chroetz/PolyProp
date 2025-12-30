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

  sprintf("Rscript run_forecast_main.R L63 ydmns 1:8 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ydmns 9:12 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ydmns 13:14 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ydmns 15 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ydmns 16 %s %s", runMode, randomSeeds),

  NULL
)

runCmdsLocal(cmds, parallel=FALSE)
