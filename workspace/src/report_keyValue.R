options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")



filePaths <- list.files(evalDirPath, pattern="^solverError_eval_.*\\.json", full.names=TRUE)
solverError <-
  lapply(filePaths, \(filePath) {
    info <- readInfo(filePath = filePath)
    tibble(
      system = info$systemLabel,
      solverPrecision = info$solverPrecision,
      truthPrecision = str_sub(info$truthName, start=-2, end=-1),
      mean = info$vpt$mean,
      ci95low = info$vpt$confInt95[1],
      ci95high = info$vpt$confInt95[2],
      n = info$vpt$n
    )
  }) |>
  bind_rows()

solverErrorKeyValue <-
  solverError |>
  filter(truthPrecision == "md") |>
  mutate(keyPrefix = sprintf("solverErr_%s_%s", system, solverPrecision)) |>
  select(keyPrefix, mean, n, ci95low, ci95high) |>
  pivot_longer(c(mean, n, ci95low, ci95high)) |>
  mutate(key = paste0(keyPrefix, "_", name)) |>
  select(key, value)

systemLabels <- c("L63", "TCSA", paste0("L96D", 5:9))
lyapunovKeyValue <-
  lapply(
    systemLabels,
    \(systemLabel) {
      lyapunovInfo <- readInfo(evalDirPath, sprintf("lyapunov_eval_%s_d", systemLabel))
      tribble(
        ~key, ~value,
        sprintf("lyapunov_%s_mean", systemLabel), lyapunovInfo$lyapunov$mean,
        sprintf("lyapunov_%s_n", systemLabel), lyapunovInfo$lyapunov$n,
        sprintf("lyapunov_%s_ci95low", systemLabel), lyapunovInfo$lyapunov$confInt95[1],
        sprintf("lyapunov_%s_ci95high", systemLabel), lyapunovInfo$lyapunov$confInt95[2],
      )
    }
  ) |>
  bind_rows()


keyValue <-
  bind_rows(
    solverErrorKeyValue |> mutate(value = as.character(value)),
    lyapunovKeyValue |> mutate(value = as.character(value))
  )


keyValue |> write_csv(file.path(keyValueDirPath, "keyValue.csv"))
