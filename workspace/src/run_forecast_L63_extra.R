source("common_run.R")
source("common_defaults.R")

default <- getDefaults()

args <- commandArgs(TRUE)
if (length(args) == 0) {
  runMode <- "s"
  randomSeeds <- 1
} else {
  stopifnot(length(args)==2)
  runMode <- args[1]
  randomSeeds <- eval(parse(text = args[2]))
}



cmdBase <- makeCmd(
  "truth_and_forecast_mmm.R",
  c(
    "randomSeed=%d",
    sprintf("solverTimeStep=%.17g", default$L63$timeStep),
    "testDuration=500",
    sprintf("nObs=%s", default$forecast$nObs),
    sprintf("nSteps=%s", default$forecast$nSteps),
    "nDegs=%s"
  )
)


cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "1:8"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "9:12"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "13:14"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "15"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "16"))
}



runCmds(cmds, runMode, "L63_mmm", qos="short", timeInMin=1440, memInGb=10)

