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
  "forecast.R",
  c(
    "randomSeed=%d",
    "truthName='TCSA_md'",
    "testMode='sequential'",
    sprintf("testDuration=%d", default$TCSA$testDuration),
    "nObs=%s",
    "nSteps=16",
    "nDegs=%d",
    "normalization='none'",
    "methodPrecision='m'"
  )
)


nObs <- "2^16"
cmds <- NULL
for (randomSeed in randomSeeds) {
  for (nDegs in 23:24) {
    cmds <- c(cmds, sprintf(cmdBase, randomSeed, nObs, nDegs))
  }
}
runCmds(cmds, runMode, "TCSA_e1", qos="standby", timeInMin=2*1440, memInGb=24)

nDegs <- 25
cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, nObs, nDegs))
}
runCmds(cmds, runMode, "TCSA_e2", qos="standby", timeInMin=3*1440, memInGb=28)

nObs <- "2^17"
cmds <- NULL
for (randomSeed in randomSeeds) {
  for (nDegs in 23:24) {
    cmds <- c(cmds, sprintf(cmdBase, randomSeed, nObs, nDegs))
  }
}
runCmds(cmds, runMode, "TCSA_e3", qos="standby", timeInMin=3*1440, memInGb=44)

nDegs <- 25
cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, nObs, nDegs))
}
runCmds(cmds, runMode, "TCSA_e4", qos="standby", timeInMin=4*1440, memInGb=50)


