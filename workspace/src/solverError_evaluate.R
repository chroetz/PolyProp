options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_errorMetrics.R")


# default parameters
truthName <- "L63_md"
systemLabel <- NULL
solverPrecision <- "s"
quantileProbs <- c(0, 0.25, 0.5, 0.75, 1)
vptTheshold <- 0.5
lyapunov <- NULL


# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))


# set derived values
outName <- paste("solverError_eval", truthName, solverPrecision, sep="_")
outFilePath <- file.path(evalDirPath, paste0(outName, ".json"))
if (is.null(systemLabel)) systemLabel <- str_extract(truthName, "^[^_]*")
if (is.null(lyapunov)) {
  lyapunovInfo <- readInfo(evalDirPath, sprintf("lyapunov_eval_%s_d", systemLabel))
  lyapunov <- lyapunovInfo$lyapunov$mean
}



truth <- readInfoAndData(truthDirPath, truthName)
mean0 <- colMeans(truth$data[,-1])
err0 <- sqrt(mean(rowSums((truth$data[,-1]-rep(mean0, each=nrow(truth$data)))^2)))


fileBaseNames <-
  list.files(
    solverErrorDirPath,
    pattern = sprintf("^solverError_%s_%s_\\d+_[0-9a-f]{32}\\.[^.]*$", truthName, solverPrecision)
  ) |>
  str_remove("\\.[^.]+$") |>
  unique() |>
  sort()


if (length(fileBaseNames) == 0) stop("Did not find any solverError results.")

solveErrorVpt <-
  lapply(fileBaseNames, \(fileBaseName) {
    cat("Processing", fileBaseName, "...\n")
    result <- readInfoAndData(solverErrorDirPath, fileBaseName)
    time <- seq(0, result$info$testDuration, by=result$info$timeStep)
    apply(result$data, 2, validPredictionTime, time=time, thresh=vptTheshold, err0=err0 , lyapunov=lyapunov)
  }) |>
  unlist() |>
  unname()

vpt <- lst(
  mean = mean(solveErrorVpt),
  quantiles = quantile(solveErrorVpt, quantileProbs),
  sd = sd(solveErrorVpt),
  n = length(solveErrorVpt),
  confInt95 = mean + c(-1,1)*qnorm(0.975)*sd/sqrt(n),
)


info <- lst(
  args,
  truthName,
  systemLabel,
  solverPrecision,
  quantileProbs,
  vptTheshold,
  lyapunov,
  vpt,
  fileBaseNames
)


writeInfo(info, outFilePath)

