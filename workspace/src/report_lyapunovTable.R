options(tidyverse.quiet = TRUE)
library(tidyverse)
library(gt)
source("common_paths.R")
source("common_io.R")


systemLabels <- c("L63", "TCSA", paste0("L96D", 5:9))
lyapunovData <-
  lapply(
    systemLabels,
    \(systemLabel) {
      lyapunovInfo <- readInfo(evalDirPath, sprintf("lyapunov_eval_%s_d", systemLabel))
      tibble(
        systemLabel = systemLabel,
        mean = lyapunovInfo$lyapunov$mean,
        ci95Lower = lyapunovInfo$lyapunov$confInt95[1],
        ci95Upper = lyapunovInfo$lyapunov$confInt95[2]
      )
    }
  ) |>
  bind_rows()

tbl <-
  lyapunovData |>
  mutate(ci95 = sprintf("[%.5g, %.5g]", ci95Lower, ci95Upper)) |>
  select(systemLabel, mean, ci95) |>
  gt() |>
  tab_options(table_body.hlines.style = "none") |>
  cols_label(
    systemLabel = "System",
    mean = I("$\\lambda_{\\mathsf{max}}$"),
    ci95 = "95%-Conf.Int."
  ) |>
  fmt_number(columns=mean, n_sigfig=5) |>
  cols_align(columns=ci95, align="center")

writeTableAsLatex(tbl, file.path(tableDirPath, "Lyapunov_table.tex"))
