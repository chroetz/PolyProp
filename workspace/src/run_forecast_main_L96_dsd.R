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
    "truthName='L96D%d_ds'",
    "testMode='sequential'",
    sprintf("testDuration=%d", default$L96$testDuration),
    "nObs=%s",
    "nSteps=2^(1:5)",
    "nDegs=%s",
    "normalization='full'",
    "methodPrecision='d'"
  )
)




cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 5, "2^(3:15)", "1:8"))
}
runCmds(cmds, runMode, "L96D5_dsd_f_s", qos="short", timeInMin=60)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 6, "2^(3:15)", "1:8"))
}
runCmds(cmds, runMode, "L96D6_dsd_f_s", qos="short", timeInMin=60)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 7, "2^(3:15)", "1:8"))
}
runCmds(cmds, runMode, "L96D7_dsd_f_s", qos="short", timeInMin=60)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 8, "2^(3:15)", "1:8"))
}
runCmds(cmds, runMode, "L96D8_dsd_f_s", qos="short", timeInMin=1440, memInGb=10)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 9, "2^(3:15)", "1:8"))
}
runCmds(cmds, runMode, "L96D9_dsd_f_s", qos="short", timeInMin=1440, memInGb=24)



cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 5, "2^(16:17)", "1:8"))
}
runCmds(cmds, runMode, "L96D5_dsd_f_s", qos="short", timeInMin=60)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 6, "2^(16:17)", "1:8"))
}
runCmds(cmds, runMode, "L96D6_dsd_f_s", qos="short", timeInMin=60)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 7, "2^16", "1:8"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 7, "2^17", "1:8"))
}
runCmds(cmds, runMode, "L96D7_dsd_f_s", qos="short", timeInMin=60, memInGb=10)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 8, "2^16", "1:8"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 8, "2^17", "1:8"))
}
runCmds(cmds, runMode, "L96D8_dsd_f_s", qos="short", timeInMin=1440, memInGb=20)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 9, "2^16", "1:8"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 9, "2^17", "1:7"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, 9, "2^17", "8"))
}
runCmds(cmds, runMode, "L96D9_dsd_f_s", qos="short", timeInMin=1440, memInGb=50)
