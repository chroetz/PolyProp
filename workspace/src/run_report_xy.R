source("common_run.R")
source("common_defaults.R")
source("common_paths.R")

default <- getDefaults()

args <- commandArgs(TRUE)
if (length(args) == 0) {
  runMode <- "s"
} else if (length(args)==1) {
  runMode <- args[1]
} else {
  runMode <- args[1]
}

cmds <- paste0(
  "Rscript report_",
  c(
    "xy",
    NULL
  ),
  ".R"
)


runCmds(cmds, runMode, "report", qos="short", timeInMin=20)

