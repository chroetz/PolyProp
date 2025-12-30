source("common_run.R")
source("common_defaults.R")
source("common_paths.R")

default <- getDefaults()

args <- commandArgs(TRUE)
if (length(args) == 0) {
  runMode <- "s"
  folders <- NULL
} else if (length(args)==1) {
  runMode <- args[1]
  folders <- NULL
} else {
  runMode <- args[1]
  folders <- args[-1]
}


cmdBase <- makeCmd(
  "evaluate.R",
  c(
    "folder='%s'"
  )
)

folders <- c("L63_xdm_n_s", "L63_ydm_n_s")
cmds <- NULL
for (folder in folders) {
  cmds <- c(cmds, sprintf(cmdBase, folder))
}


runCmds(cmds, runMode, "evaluate", qos="short", timeInMin=60, memInGb=12)

