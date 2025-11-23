source("common_run.R")
source("common_defaults.R")

default <- getDefaults()

runMode <- "c"
#
# randomSeeds <- c(5L, 7L, 12L, 15L, 16L, 18L, 19L, 23L, 28L, 31L, 32L, 33L, 34L,
# 35L, 36L, 37L, 38L, 46L)
#
# cmds <-  sprintf("Rscript run_forecast_main.R L63 mdmnr 16 %s %s", runMode, randomSeeds)
#
# runCmdsLocal(cmds, parallel=FALSE)
#


randomSeeds <- c(24L, 34L, 37L, 38L, 40L, 44L, 45L, 46L, 51L, 52L, 53L, 54L,
55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L, 67L,
68L, 80L, 81L, 87L, 88L, 89L, 96L, 97L, 98L)

cmdBase <- makeCmd(
  "truth_and_forecast_mmm.R",
  c(
    "randomSeed=%d",
    sprintf("solverTimeStep=%.17g", default$L63$timeStep),
    "testDuration=500",
    sprintf("nObs=%s", default$forecast$nObs),
    sprintf("nSteps=%s", default$forecast$nSteps),
    "nDegs=%s"
  )
)

cmds <- NULL
for (randomSeed in randomSeeds) {
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "1:8"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "9:12"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "13:14"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "15"))
  cmds <- c(cmds, sprintf(cmdBase, randomSeed, "16"))
}


runCmds(cmds, runMode, "L63_mmm", qos="short", timeInMin=1440, memInGb=10)

