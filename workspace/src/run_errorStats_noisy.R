source("common_run.R")
source("common_defaults.R")
source("common_paths.R")

default <- getDefaults()

args <- commandArgs(TRUE)
if (length(args) == 0) {
  runMode <- "s"
  errorMetrics <- c("vpt0.1", "vpt0.3", "vpt0.5", "vpt1.0", "nPerfect", "cvs", "nrmse1")
} else if (length(args)==1) {
  runMode <- args[1]
  errorMetrics <- c("vpt0.1", "vpt0.3", "vpt0.5", "vpt1.0", "nPerfect", "cvs", "nrmse1")
} else {
  runMode <- args[1]
  errorMetrics <- args[-1]
}


cmdBase <- makeCmd(
  "errorStats_noisy.R",
  c(
    "errorMetric='%s'"
  )
)


cmds <- NULL
for (errorMetric in errorMetrics) {
  cmds <- c(cmds, sprintf(cmdBase, errorMetric))
}


runCmds(cmds, runMode, "errorStats", qos="short", timeInMin=10)
