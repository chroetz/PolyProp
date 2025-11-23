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
  "truth.R",
  c("duration=%.17g",
    "timeStep=%.17g",
    "systemLabel='%s'",
    "solverPrecision='%s'"
  )
)

cmds <- c(
  sprintf(cmdBase, default$L63$truthDuration, default$L63$timeStep, "L63", "s"),
  sprintf(cmdBase, default$L63$truthDuration, default$L63$timeStep, "L63", "d"),
  sprintf(cmdBase, default$L63$truthDuration, default$L63$timeStep, "L63", "m"),
  sprintf(cmdBase, default$TCSA$truthDuration, default$TCSA$timeStep, "TCSA", "s"),
  sprintf(cmdBase, default$TCSA$truthDuration, default$TCSA$timeStep, "TCSA", "d"),
  sprintf(cmdBase, default$TCSA$truthDuration, default$TCSA$timeStep, "TCSA", "m"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D5", "s"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D6", "s"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D7", "s"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D8", "s"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D9", "s"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D5", "d"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D6", "d"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D7", "d"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D8", "d"),
  sprintf(cmdBase, default$L96$truthDuration, default$L96$timeStep, "L96D9", "d"),
  NULL
)



runCmds(cmds, runMode, label="truth", qos="short", timeInMin=30)

