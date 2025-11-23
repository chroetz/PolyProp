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
  "truth_32bit.R",
  "truthName='%s'"
)

cmds <- c(
  sprintf(cmdBase, "L63_dd"),
  sprintf(cmdBase, "L63_md"),
  sprintf(cmdBase, "L96D5_dd"),
  sprintf(cmdBase, "L96D6_dd"),
  sprintf(cmdBase, "L96D7_dd"),
  sprintf(cmdBase, "L96D8_dd"),
  sprintf(cmdBase, "L96D9_dd")
)



runCmds(cmds, runMode, label="truth", qos="short", timeInMin=10)

