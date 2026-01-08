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

truthName <- "L63_dd"
testMode <- "sequential"
testDuration <- default$L63$testDuration
normalization <- "full"
methodPrecision <- "d"
nDegs <- "1:8"
noiseLevels <- "c(0,1e-9,1e-7,1e-5,1e-3,1e-1)"

initalNoise <- "TRUE"

cmdBase <- makeCmd(
  "forecast_noisy.R",
  c(
    "randomSeed=%d",
    sprintf("truthName='%s'", truthName),
    sprintf("testMode='%s'", testMode),
    sprintf("testDuration=%d", testDuration),
    sprintf("nObs=%s", default$forecast$nObs),
    sprintf("nSteps=%s", default$forecast$nSteps),
    sprintf("nDegs=%s", nDegs),
    sprintf("normalization='%s'", normalization),
    sprintf("methodPrecision='%s'", methodPrecision),
    sprintf("noiseLevels=%s", noiseLevels),
    sprintf("initalNoise=%s", initalNoise)
  )
)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed))
}

runCmds(cmds, runMode, "L63_noisy", qos="short", timeInMin=1440)


initalNoise <- "FALSE"

cmdBase <- makeCmd(
  "forecast_noisy.R",
  c(
    "randomSeed=%d",
    sprintf("truthName='%s'", truthName),
    sprintf("testMode='%s'", testMode),
    sprintf("testDuration=%d", testDuration),
    sprintf("nObs=%s", default$forecast$nObs),
    sprintf("nSteps=%s", default$forecast$nSteps),
    sprintf("nDegs=%s", nDegs),
    sprintf("normalization='%s'", normalization),
    sprintf("methodPrecision='%s'", methodPrecision),
    sprintf("noiseLevels=%s", noiseLevels),
    sprintf("initalNoise=%s", initalNoise)
  )
)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed))
}

runCmds(cmds, runMode, "L63_noisy", qos="short", timeInMin=1440)

