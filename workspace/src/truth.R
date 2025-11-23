source("common_paths.R")
source("common_io.R")
source("common_integrate.R")
source("common_defaults.R")


# default parameters
defaults <- getDefaults()
duration <- 100
timeStep <- 2^(-10)
systemLabel <- "L63" # "L63", "TCSA"
solverPrecision <- "s" # "s", "d", "m"
outName <- NULL
warmUpDuration <- NULL

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived values
precisionLabel <- sprintf("%sd", solverPrecision) # always store data at double precision -> storage precision label: d
if (is.null(outName)) outName <- paste(systemLabel, precisionLabel, sep="_")
n <- round(duration / timeStep) + 1
if (is.null(warmUpDuration)) {
  warmUpDuration <- duration * 0.1
}
nWarmUp <- round(warmUpDuration / timeStep)


integrateSystem <- getIntegrateSystem(systemLabel, solverPrecision)
state0 <- defaults[[systemLabel]]$state0
dataAll <- integrateSystem(state0, timeStep, n + nWarmUp)
dataWarm <- dataAll[-seq_len(nWarmUp), ]


info <- dplyr::lst(
  args,
  duration,
  timeStep,
  n,
  systemLabel,
  solverPrecision,
  precisionLabel,
  warmUpDuration,
  nWarmUp,
  state0
)


writeInfoAndData(info, dataWarm, truthDirPath, outName)

