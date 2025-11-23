source("common_run.R")
source("common_defaults.R")

default <- getDefaults()

args <- commandArgs(TRUE)
if (length(args) == 0) { # L63 dddns 1:8 s 1
  runMode <- "s"
  randomSeeds <- 1
  truthName <- "L63_dd"
  testMode <- "sequential"
  testDuration <- default$L63$testDuration
  normalization <- "none"
  methodPrecision <- "d"
  label <- "L63_dddns"
  nDegs <- "1:8"
} else {
  stopifnot(length(args)==5)
  systemLabel <- args[1]
  mode <- strsplit(args[2], "")[[1]]
  stopifnot(length(mode) == 5)
  truthName <- paste0(systemLabel, "_", mode[1], mode[2])
  testMode <- switch(
    mode[5],
    s = "sequential",
    r = "random"
  )
  testDuration <- default[[systemLabel]]$testDuration
  normalization <- switch(
    mode[4],
    n = "none",
    d = "diag",
    f = "full"
  )
  methodPrecision <- mode[3]
  label <- paste0(systemLabel, "_", args[2])
  nDegs <- args[3]
  runMode <- args[4]
  randomSeeds <- eval(parse(text = args[5]))
}


cmdBase <- makeCmd(
  "forecast.R",
  c(
    "randomSeed=%d",
    sprintf("truthName='%s'", truthName),
    sprintf("testMode='%s'", testMode),
    sprintf("testDuration=%d", testDuration),
    sprintf("nObs=%s", default$forecast$nObs),
    sprintf("nSteps=%s", default$forecast$nSteps),
    sprintf("nDegs=%s", nDegs),
    sprintf("normalization='%s'", normalization),
    sprintf("methodPrecision='%s'", methodPrecision)
  )
)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed))
}


runCmds(cmds, runMode, label, qos="short", timeInMin=1440)

