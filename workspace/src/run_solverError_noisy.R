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
  "solverError_noisy.R",
  c(
    "randomSeed=%d",
    "truthName='%s'",
    "timeStep=%g",
    "testDuration=%d",
    "solverPrecision='%s'",
    "noiseLevel=%g",
    "nReps=100"
  )
)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_dd", default$L63$timeStep, default$L63$testDuration, "d", 1e-9))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_dd", default$L63$timeStep, default$L63$testDuration, "d", 1e-7))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_dd", default$L63$timeStep, default$L63$testDuration, "d", 1e-5))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_dd", default$L63$timeStep, default$L63$testDuration, "d", 1e-3))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_dd", default$L63$timeStep, default$L63$testDuration, "d", 1e-1))
}
runCmds(cmds, runMode, label="solErrNoi", qos="short", timeInMin=60)
