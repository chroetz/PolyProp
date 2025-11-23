options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")

label <- "L63_mdm_n_s"

systemLabel <- str_extract(label, "^[^_]*")
truthName <- str_extract(label, "^[^_]*_.{2}")

lyapunovInfo <- readInfo(evalDirPath, sprintf("lyapunov_eval_%s_d", systemLabel))
lyapunov <- lyapunovInfo$lyapunov$mean

truth <- readInfoAndData(truthDirPath, truthName)
mean0 <- colMeans(truth$data[,-1])
err0 <- sqrt(mean(rowSums((truth$data[,-1]-rep(mean0, each=nrow(truth$data)))^2)))

evaluationFilePath <- file.path(evalDirPath, sprintf("forecast_eval_%s.csv", label))
error <- read_csv(evaluationFilePath)
bestRun <-
  error |>
  filter(nSteps > 1) |>
  filter(nPerfect*nSteps == max(nPerfect*nSteps, na.rm=TRUE)) |>
  filter(n() == 1)

resultDirPath <- file.path(forecastDirPath, label)
resultPrefix <- sprintf("%s_%04d", label, bestRun$randomSeed)
resultNames <-
  list.files(
    resultDirPath,
    pattern = sprintf("^%s_[0-9a-f]{32}\\.[^.]*$", resultPrefix)
  ) |>
  str_remove("\\.[^.]+$") |>
  unique() |>
  sort()

evaluationData <-
  lapply(resultNames, readData, dirPath = resultDirPath) |>
  bind_rows()

bestEvaluationData <-
  evaluationData |>
  semi_join(bestRun, join_by(nObs, nSteps, nDegs))

stopifnot(nrow(bestEvaluationData) == 1)

errorData <-
  bestEvaluationData$result[[1]] |>
  mutate(time = time * lyapunov)

fitData <- errorData |> filter(time >= 12, time < 41)
y <- fitData |> pull(error) |> log()
x <- fitData |> pull(time)
fit <- lm(y ~ x)

fit

plt <-
  errorData |>
  ggplot(aes(x = time, y = error)) +
  scale_y_log10() +
  geom_vline(xintercept = errorData$time[which(errorData$error == 0)], color=3) +
  annotate("text", label = "Perfect Prediction", x = 8, y = 3e-3, color = 3, hjust = "left") +
  geom_hline(yintercept = .Machine$double.eps, color = "#777") +
  annotate("text", label = "64bit Machine Eps", x = 25, y = 1e-15, color = "#777") +
  geom_hline(yintercept = err0*0.5, color = 2) +
  annotate("text", label = "$\\mathsf{VPT}_{0.5}$ threshold", x = 20, y = 3e1, color = 2, hjust = "left") +
  geom_vline(xintercept = bestRun$vpt0.5, color = 2) +
  annotate("text", label = "$\\mathsf{VPT}_{0.5}$", x = 43, y = 3e-17, color = 2, hjust = "left") +
  geom_line(data = tibble(time = x, error = exp(fit$fitted.values)), color=4, linewidth=2) +
  annotate("text", label = sprintf("log-scale slope: %.2f", fit$coefficients[2]), x = 27, y = 4e-8, color = 4, hjust = "left") +
  geom_line() +
  ylab("Error [Euclidean norm]") +
  xlab("Forecast Time [Lyapunov times]")

saveGgplotAsTikz(plt, file.path(plotDirPath, "perfect.tex"), width = 8, height = 4)

