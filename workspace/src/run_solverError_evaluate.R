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
cmds <- c(cmds, sprintf(cmdBase, "L63_ds", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L63_ms", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L63_sd", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L63_sd", "m"))
cmds <- c(cmds, sprintf(cmdBase, "L63_dd", "s"))
cmds <- c(cmds, sprintf(cmdBase, "L63_dd", "m"))
cmds <- c(cmds, sprintf(cmdBase, "L63_md", "s"))
cmds <- c(cmds, sprintf(cmdBase, "L63_md", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L63_md", "m"))
cmds <- c(cmds, sprintf(cmdBase, "TCSA_md", "s"))
cmds <- c(cmds, sprintf(cmdBase, "TCSA_md", "m"))
cmds <- c(cmds, sprintf(cmdBase, "TCSA_md", "d"))

runCmds(cmds, runMode, label="solErrEva", qos="short", timeInMin=20)


cmds <- NULL
cmds <- c(cmds, sprintf(cmdBase, "L96D5_sd", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L96D6_sd", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L96D7_sd", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L96D8_sd", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L96D9_sd", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L96D5_ds", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L96D6_ds", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L96D7_ds", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L96D8_ds", "d"))
cmds <- c(cmds, sprintf(cmdBase, "L96D9_ds", "d"))

runCmds(cmds, runMode, label="solErrEva", qos="short", timeInMin=20)

