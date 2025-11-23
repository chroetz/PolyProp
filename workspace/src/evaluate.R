options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_errorMetrics.R")


# default parameters
folder <- "L63_mdd_f_s"
lyapunov <- NULL
vptThresholds <- c(0.1, 0.3, 0.5, 1)
label <- NULL
truthName <- NULL
systemLabel <- NULL

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived values
baseName <- str_remove(folder, "^noisy_")
if (is.null(label)) label <- baseName
if (is.null(truthName)) truthName <- str_extract(baseName, "^[^_]*_.{2}")
if (is.null(systemLabel)) systemLabel <- str_extract(truthName, "^[^_]*")
if (str_detect(truthName, "_mm$")) truthName <- paste0(systemLabel, "_md") # no <system>_mm truth is stored; default to <system>_md
resultDirPath <- file.path(forecastDirPath, folder)
evaluationFilePath <- file.path(evalDirPath, sprintf("forecast_eval_%s.csv", folder))
if (is.null(lyapunov)) {
  lyapunovInfo <- readInfo(evalDirPath, sprintf("lyapunov_eval_%s_d", systemLabel))
  lyapunov <- lyapunovInfo$lyapunov$mean
}


resultNames <-
  list.files(
    resultDirPath,
    pattern = sprintf("^%s_[0-9]+_[0-9a-f]{32}\\.[^.]*$", folder)
  ) |>
  str_remove("\\.[^.]+$") |>
  unique() |>
  sort()

if (length(resultNames) == 0) stop("Did not find any forecast results.")

truth <- readInfoAndData(truthDirPath, truthName)
mean0 <- colMeans(truth$data[,-1])
err0 <- sqrt(mean(rowSums((truth$data[,-1]-rep(mean0, each=nrow(truth$data)))^2)))

evaluationData <-
  lapply(resultNames, \(resultName) {

    cat("Processing", resultName, "...\n")

    result <- readInfoAndData(resultDirPath, resultName)

    vpt <- sapply(vptThresholds, \(thresh) sapply(result$data$result, \(res) {
      validPredictionTime(res$error, res$time, thresh = thresh, err0 = err0, lyapunov = lyapunov)
    }))
    if (length(vpt) == 0) {
      warning("No values data found in ", resultName, immediate.=TRUE)
      return(NULL)
    }
    if (is.null(dim(vpt))) {
      dim(vpt) <- c(1, length(vpt))
    }
    colnames(vpt) <- sprintf("vpt%.1f", vptThresholds)

    nPerfect <- sapply(
      result$data$result,
      \(res) numberOfPerfectPredictions(res$error)
    )

    cvs <- sapply(
      result$data$result,
      \(res) cumulativeValidityScore(res$error, res$time, thresh = 1, err0 = err0, lyapunov = lyapunov)
    )

    nrmse1 <- sapply(
      result$data$result,
      \(res) oneLyapunovTimeNrmse(res$error, res$time, err0 = err0, lyapunov = lyapunov)
    )

    precisonLabels <- str_split_1(result$info$precisionLabel, "")

    result$data |>
      select(-result, -idx) |>
      bind_cols(
        vpt,
        nPerfect = nPerfect,
        cvs = cvs,
        nrmse1 = nrmse1
      ) |>
      mutate(
        randomSeed = result$info$randomSeed,
        system = result$info$systemLabel,
        precisionLabel = result$info$precisionLabel,
        systemPrecision = precisonLabels[1],
        dataPrecision = precisonLabels[2],
        methodPrecision = precisonLabels[3],
        normalization = result$info$normalization,
        testMode = result$info$testMode,
        name = result$info$name,
        hash = result$info$hash,
        .after=nDegs
      )
  }) |>
  bind_rows()


write_csv(evaluationData, evaluationFilePath)

