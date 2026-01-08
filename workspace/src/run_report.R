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
    "detail",
    "detail_noisy",
    "special",
    "bestTable",
    "bestTable_L96plot",
    "keyValue",
    "trajectory",
    "perfect",
    "noisy",
    "parameters",
    NULL
  ),
  ".R"
)
cmds <- c(
  cmds,
  "Rscript report_attractor.R \"systemLabel='L63'\" \"warmUpDuration=10\" \"duration=20\" \"swapAxis=FALSE\"",
  "Rscript report_attractor.R \"systemLabel='TCSA'\" \"warmUpDuration=100\" \"duration=200\" \"swapAxis=TRUE\"",
  "Rscript report_attractor.R \"systemLabel='L96D5'\" \"warmUpDuration=10\" \"duration=20\" \"swapAxis=FALSE\"",
  "Rscript report_attractor.R \"systemLabel='L96D6'\" \"warmUpDuration=10\" \"duration=20\" \"swapAxis=FALSE\"",
  "Rscript report_attractor.R \"systemLabel='L96D7'\" \"warmUpDuration=10\" \"duration=20\" \"swapAxis=FALSE\"",
  "Rscript report_attractor.R \"systemLabel='L96D8'\" \"warmUpDuration=10\" \"duration=20\" \"swapAxis=FALSE\"",
  "Rscript report_attractor.R \"systemLabel='L96D9'\" \"warmUpDuration=10\" \"duration=20\" \"swapAxis=FALSE\""
)

cmds <- c(
  cmds,
  "Rscript report_trajectory_512bit.R \"n=1024\" \"step=16\" \"deg=16\"",
  NULL
)

runCmds(cmds, runMode, "report", qos="short", timeInMin=20)



cmds <- c(
  "Rscript report_trajectory_512bit.R \"n=32768\" \"step=2\" \"deg=16\"",
  NULL
)

runCmds(cmds, runMode, "report", qos="short", timeInMin=300, memInGb=10)

