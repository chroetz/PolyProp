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
  # ddd X s with different normalization X
  sprintf("Rscript run_forecast_main.R L63 dddns 1:16 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 dddds 1:16 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 dddfs 1:16 %s %s", runMode, randomSeeds),

  # dds X s with different normalization X
  sprintf("Rscript run_forecast_main.R L63 ddsns 1:16 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ddsds 1:16 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 ddsfs 1:16 %s %s", runMode, randomSeeds),

  # sdX f s with different method precision X and default normalization f
  sprintf("Rscript run_forecast_main.R L63 sdsfs 1:16 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 sddfs 1:16 %s %s", runMode, randomSeeds),

  # sdX f s with different method precision X and default normalization f
  sprintf("Rscript run_forecast_main.R L63 mdsfs 1:16 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 mddfs 1:16 %s %s", runMode, randomSeeds),

  # Xsd f s with different solver precision X and default normalization f
  sprintf("Rscript run_forecast_main.R L63 dsdfs 1:16 %s %s", runMode, randomSeeds),
  sprintf("Rscript run_forecast_main.R L63 msdfs 1:16 %s %s", runMode, randomSeeds),

  NULL
)

runCmdsLocal(cmds, parallel=FALSE)
