source("common_paths.R")
source("common_io.R")


# default parameters
truthName <- "L63_dd"
outName <- NULL

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived values
truth <- readInfoAndData(truthDirPath, truthName)
systemLabel <- truth$info$systemLabel
precisionLabel <- sprintf("%ss", truth$info$solverPrecision) # always store data at single precision -> storage precision label: s
if (is.null(outName)) outName <- paste(systemLabel, precisionLabel, sep="_")


Rcpp::sourceCpp("convert_to_32bit.cpp")
data <- convert_to_32bit(truth$data)


info <-
  c(
    dplyr::lst(
      args,
      precisionLabel,
    ),
    truth$info[c(
      "solverPrecision",
      "duration",
      "timeStep",
      "n",
      "systemLabel",
      "warmUpDuration",
      "nWarmUp",
      "state0"
    )]
  )


writeInfoAndData(info, data, truthDirPath, outName)

