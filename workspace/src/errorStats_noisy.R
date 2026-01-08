options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")


# default parameters
errorMetric <- "vpt0.5"

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived values
evaluationFilePaths <- list.files(evalDirPath, pattern="^forecast_eval_noisy.*\\.csv$", full.names=TRUE)
outName <- sprintf("errorStats_noisy_%s.csv", errorMetric)



errorLong <-
  lapply(
    evaluationFilePaths,
    \(evaluationFilePath) {
      evaluationRaw <- read_csv(evaluationFilePath, col_types=cols())
      evaluationRaw |>
        select(
          system, systemPrecision, dataPrecision, testMode,
          methodPrecision, normalization, nObs, nSteps, nDegs, noiseLevels, randomSeed,
          all_of(errorMetric)
        ) |>
        mutate(name = str_sub(basename(evaluationFilePath), start=21, end=-5)) |>
        mutate(initalNoise = str_detect(name, "TRUE$")) |>
        rename(error = all_of(errorMetric)) |>
        distinct() |>
        arrange(nObs, nSteps, nDegs, noiseLevels, randomSeed)
    }
  ) |>
  bind_rows()

errorStats <-
  errorLong |>
  summarise(
    mean = mean(error, na.rm=TRUE),
    sd = sd(error, na.rm=TRUE),
    min = min(error, na.rm=TRUE),
    q05 = quantile(error, probs=0.05, na.rm=TRUE),
    q10 = quantile(error, probs=0.10, na.rm=TRUE),
    q20 = quantile(error, probs=0.20, na.rm=TRUE),
    q25 = quantile(error, probs=0.25, na.rm=TRUE),
    median = median(error, na.rm=TRUE),
    q75 = quantile(error, probs=0.75, na.rm=TRUE),
    q80 = quantile(error, probs=0.80, na.rm=TRUE),
    q90 = quantile(error, probs=0.90, na.rm=TRUE),
    q95 = quantile(error, probs=0.95, na.rm=TRUE),
    max = max(error, na.rm=TRUE),
    nNa = sum(is.na(error)),
    n = n(),
    ci95Lower = mean - qnorm(0.975) * sd / sqrt(n-nNa),
    ci95Upper = mean + qnorm(0.975) * sd / sqrt(n-nNa),
    .by = c(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, nObs, nSteps, nDegs, noiseLevels, initalNoise)
  )

write_csv(errorStats, file.path(evalDirPath, outName))

