source("common_run.R")
source("common_defaults.R")

default <- getDefaults()

runMode <- "s"
randomSeeds <- 1
truthName <- "L63_dd"
testMode <- "sequential"
testDuration <- default$L63$testDuration
normalization <- "full"
methodPrecision <- "d"
nDegs <- "1:8"
noiseLevels <- "c(0,1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1e0)"

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
    sprintf("noiseLevels=%s", noiseLevels)
  )
)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed))
}


runCmds(cmds, runMode, "L63_noisy", qos="short", timeInMin=1440)

