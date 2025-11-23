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
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_ds", default$L63$timeStep, default$L63$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_ms", default$L63$timeStep, default$L63$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_sd", default$L63$timeStep, default$L63$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_sd", default$L63$timeStep, default$L63$testDuration, "m"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_dd", default$L63$timeStep, default$L63$testDuration, "s"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_dd", default$L63$timeStep, default$L63$testDuration, "m"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_md", default$L63$timeStep, default$L63$testDuration, "s"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_md", default$L63$timeStep, default$L63$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L63_md", default$L63$timeStep, default$L63$testDuration, "m"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "TCSA_md", default$TCSA$timeStep, default$TCSA$testDuration, "s"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "TCSA_md", default$TCSA$timeStep, default$TCSA$testDuration, "m"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "TCSA_md", default$TCSA$timeStep, default$TCSA$testDuration, "d"))
}
runCmds(cmds, runMode, label="solverError", qos="short", timeInMin=400)


cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D5_sd", default$L96$timeStep, default$L96$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D6_sd", default$L96$timeStep, default$L96$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D7_sd", default$L96$timeStep, default$L96$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D8_sd", default$L96$timeStep, default$L96$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D9_sd", default$L96$timeStep, default$L96$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D5_ds", default$L96$timeStep, default$L96$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D6_ds", default$L96$timeStep, default$L96$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D7_ds", default$L96$timeStep, default$L96$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D8_ds", default$L96$timeStep, default$L96$testDuration, "d"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "L96D9_ds", default$L96$timeStep, default$L96$testDuration, "d"))
}
runCmds(cmds, runMode, label="solverError", qos="short", timeInMin=60)
