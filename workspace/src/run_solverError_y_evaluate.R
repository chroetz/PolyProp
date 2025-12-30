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
  "solverError_evaluate.R",
  c(
    "truthName='%s'",
    "solverPrecision='%s'"
  )
)

cmds <- NULL
cmds <- c(cmds, sprintf(cmdBase, "L63_xd", "m"))
cmds <- c(cmds, sprintf(cmdBase, "L63_yd", "m"))
cmds <- c(cmds, sprintf(cmdBase, "L63_xd", "x"))
# cmds <- c(cmds, sprintf(cmdBase, "L63_yd", "x"))
cmds <- c(cmds, sprintf(cmdBase, "L63_xd", "y"))
cmds <- c(cmds, sprintf(cmdBase, "L63_yd", "y"))

runCmds(cmds, runMode, label="solErrEva", qos="short", timeInMin=20)
