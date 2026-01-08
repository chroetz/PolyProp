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
  "solverError_noisy_evaluate.R",
  c(
    "truthName='%s'",
    "solverPrecision='%s'"
  )
)

cmds <- NULL
cmds <- c(cmds, sprintf(cmdBase, "L63_dd", "d"))

runCmds(cmds, runMode, label="solErrEva", qos="short", timeInMin=20)

