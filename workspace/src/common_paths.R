dataDirPath <- "../../data"
plotDirPath <- file.path("../img")
tableDirPath <- file.path("../tbl")
keyValueDirPath <- file.path("../key")
evalDirPath <- file.path(dataDirPath, "evaluation")
forecastDirPath <- file.path(dataDirPath, "forecast")
truthDirPath <- file.path(dataDirPath, "truth")
solverErrorDirPath <- file.path(dataDirPath, "solverError")
logDirPath <- file.path(dataDirPath, "_log")

outDirPaths <- dplyr::lst(
  dataDirPath,
  plotDirPath,
  tableDirPath,
  keyValueDirPath,
  evalDirPath,
  forecastDirPath,
  truthDirPath,
  solverErrorDirPath
)

lapply(outDirPaths, dir.create, showWarnings = FALSE, recursive = TRUE)
