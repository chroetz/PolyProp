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
  "lyapunov.R",
  c(
    "truthName='%s_dd'",
    "timeStep=%.17g",
    "steps=1e5",
    "perturbationScale=1e-8",
    "nReps=1e5"
  )
)

cmds <- NULL
cmds <- c(cmds, sprintf(cmdBase, "L63", default$L63$timeStep))
cmds <- c(cmds, sprintf(cmdBase, "TCSA", default$TCSA$timeStep))
cmds <- c(cmds, sprintf(cmdBase, "L96D5", default$L96$timeStep))
cmds <- c(cmds, sprintf(cmdBase, "L96D6", default$L96$timeStep))
cmds <- c(cmds, sprintf(cmdBase, "L96D7", default$L96$timeStep))
cmds <- c(cmds, sprintf(cmdBase, "L96D8", default$L96$timeStep))
cmds <- c(cmds, sprintf(cmdBase, "L96D9", default$L96$timeStep))


runCmds(cmds, runMode, label="lyapunov", qos="short", timeInMin=300)

