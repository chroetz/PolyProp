source("common_run.R")
source("common_defaults.R")

default <- getDefaults()

args <- commandArgs(TRUE)
if (length(args) == 0) {
  runMode <- "s"
} else {
  stopifnot(length(args)==1)
  runMode <- args[1]
}


cmdBase <- makeCmd(
  "truth_y.R",
  c("duration=%.17g",
    "timeStep=%.17g",
    "nSkip=%d",
    "systemLabel='%s'",
    "solverPrecision='%s'"
  )
)

cmds <- c(
  sprintf(cmdBase, default$L63$truthDuration, default$L63$timeStep, 2^15, "L63", "y"),
  NULL
)



runCmds(cmds, runMode, label="truth", qos="long", timeInMin=60*24*30)

