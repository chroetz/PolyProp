options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")

dirPath <- file.path(forecastDirPath, "L63_ydx_n_s")
outDirPath <- file.path(forecastDirPath, "L63_ydx_n_s")
jsonDirPath <- file.path(forecastDirPath, "L63_ydm_n_s")

fileNames <- list.files(
  dirPath,
  pattern = "^L63_ydx_n_s_ti_[0-9]+\\.feather$"
)

for (fileName in fileNames) {

  randomSeed <- fileName |> str_extract("[0-9]{4}") |> as.integer()
  filePath <- file.path(dirPath, fileName)

  outName <- sprintf("L63_ydx_n_s_%04d", randomSeed)

  dataRaw <- arrow::read_feather(filePath)
  infoPolyProp <- readInfo(dirPath=jsonDirPath, name=sprintf("L63_ydm_n_s_%04d", randomSeed))

  stopifnot(dataRaw$time[1] == infoPolyProp$testStartTime)

  testDuration <- dataRaw$time |> range() |> diff()
  data <-
    dataRaw |>
    mutate(time = time - min(time)) |>
    filter(time > 0)
  outData <- tibble(
    idx = 1,
    nObs = 1,
    nSteps = 1,
    nDegs = 0,
    result = list(data)
  )
  info <- lst(
    randomSeed,
    truthName = "L63_yd",
    testDuration,
    testMode = "s",
    nObs = 1,
    nSteps = 1,
    nDegs = 0,
    normalization = "none",
    methodPrecision = "x",
    outName,
    systemLabel = "L63",
    precisionLabel = "ydx",
    trainDurationMax = infoPolyProp$trainDurationMax,
    trainStartTime = infoPolyProp$trainStartTime,
    testStartTime = infoPolyProp$testStartTime,
    testInitial = infoPolyProp$testInitial
  )

  writeInfoAndData(info, outData, outDirPath, outName)
}
