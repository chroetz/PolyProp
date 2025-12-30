source("common_paths.R")
source("common_io.R")
source("common_defaults.R")

library(tidyverse)
filePaths <- list.files(path = "truth_xd", pattern = "^L63_xd_chunk_\\d+\\.feather$", full.names=TRUE)
fileNr <- filePaths |> basename() |> str_extract("\\d+") |> as.integer()
dataAll <- lapply(filePaths[order(fileNr)], arrow::read_feather) |> bind_rows()

defaults <- getDefaults()
duration <- 2^13
timeStep <- 2^(-10)
systemLabel <- "L63"
solverPrecision <- "x"
outName <- NULL
warmUpDuration <- duration * 0.1
n <- round(duration / (timeStep)) + 1

precisionLabel <- sprintf("%sd", solverPrecision) # always store data at double precision -> storage precision label: d
if (is.null(outName)) outName <- paste(systemLabel, precisionLabel, sep="_")

nWarmUp <- round(warmUpDuration / (timeStep))
dataWarm <- dataAll[-seq_len(nWarmUp), ] |> as.matrix()
dataWarm <- dataWarm[seq_len(n), ]

stopifnot(all(is.finite(dataWarm)))

state0 <- defaults[[systemLabel]]$state0

info <- dplyr::lst(
  duration,
  timeStep = defaults[[systemLabel]]$timeStep,
  n = n,
  systemLabel,
  solverPrecision,
  precisionLabel,
  warmUpDuration,
  nWarmUp,
  state0
)

writeInfoAndData(info, dataWarm, truthDirPath, outName)

