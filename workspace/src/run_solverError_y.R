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
  "solverError.R",
  c(
    "randomSeed=%d",
    "truthName='%s'",
    "timeStep=%g",
    "testDuration=%d",
    "solverPrecision='%s'",
    "nReps=100"
  )
)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_xd", default$L63$timeStep, default$L63$testDuration, "m"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_yd", default$L63$timeStep, default$L63$testDuration, "m"))
}

runCmds(cmds, runMode, label="solverError", qos="short", timeInMin=1440)



cmdBase <- makeCmd(
  "solverError_y.R",
  c(
    "randomSeed=%d",
    "truthName='%s'",
    "timeStep=%g",
    "nSkip=%d",
    "testDuration=%d",
    "solverPrecision='%s'",
    "nReps=1"
  )
)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_xd", default$L63$timeStep, 2^15, default$L63$testDuration, "y"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_yd", default$L63$timeStep, 2^15, default$L63$testDuration, "y"))
}

runCmds(cmds, runMode, label="solverError", qos="short", timeInMin=1440)
