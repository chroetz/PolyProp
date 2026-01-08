options(tidyverse.quiet = TRUE)
library(tidyverse)
library(gt)
source("common_paths.R")
source("common_io.R")
source("common_defaults.R")



# default parameters
errorMetric <- "vpt0.5"
errorMetricLabel <- "VPT"
errorMetricLabelText <- "Valid Prediction Time [Lyapunov times]"
defaults <- getDefaults()

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

# set derived and const values
errorStatsFilePath <- file.path(evalDirPath, sprintf("errorStats_noisy_%s.csv", errorMetric))
errorStats <- read_csv(errorStatsFilePath)
colors8 <- c("#1F77E4", "#FF7F0E", "#2CA02C", "#D62728", "#BCBD00", "#920692", "#8A564B", "#FF77C2")
colors5 <- c("#FF7F0E", "#2CA02C", "#D62728", "#BCBD00", "#920692")

experiments <-
  errorStats |>
  select(system, systemPrecision, dataPrecision, testMode, methodPrecision, normalization, noiseLevels, initalNoise) |>
  distinct() |>
  filter(noiseLevels > 0)


for (i in seq_len(nrow(experiments))) {

  experiment <- experiments[i,] |> as.list()

  outFileLabel <- sprintf(
    "%s_%s%s%s_%s_%s_%s_%s_%d",
    experiment$system,
    experiment$systemPrecision,
    experiment$dataPrecision,
    experiment$methodPrecision,
    str_sub(experiment$normalization, 1, 1),
    str_sub(experiment$testMode, 1, 1),
    errorMetricLabel,
    as.character(experiment$initalNoise),
    -log10(experiment$noiseLevels)
  )

  timeStep <- defaults[[experiment$system]]$timeStep

  experiErrorStats <-
    errorStats |>
    filter(
      system == experiment$system,
      systemPrecision == experiment$systemPrecision,
      dataPrecision == experiment$dataPrecision,
      testMode == experiment$testMode,
      methodPrecision == experiment$methodPrecision,
      normalization == experiment$normalization,
      initalNoise == experiment$initalNoise,
      noiseLevels == experiment$noiseLevels
    ) |>
    select(-all_of(names(experiment)))

  plt <-
    experiErrorStats |>
    mutate(degree = as.factor(nDegs)) |>
    mutate(deltaT = factor(sprintf("$\\Delta\\!t = 2^{%d}$", log2(timeStep*nSteps))) |> fct_reorder(nSteps)) |>
    ggplot(aes(x = nObs, y = mean, color = degree)) +
    geom_line() + geom_point() +
    scale_x_log10() +
    ylim(c(0, NA)) +
    xlab("n") + ylab(errorMetricLabelText) +
    facet_wrap(vars(deltaT), ncol = 2, dir = "v")
  set.seed(1)
  saveGgplotAsTikz(plt, file.path(plotDirPath, sprintf("%s_all_plot.tex", outFileLabel)), width = 7, height = 9)


  best <-
    experiErrorStats |>
    summarise(
      deg = nDegs[which.max(mean)],
      .by = c(nObs, nSteps)
    ) |>
    left_join(
      experiErrorStats, join_by(nObs, nSteps, deg == nDegs)
    )

  nUniqueSteps <- best$nSteps |> unique() |> length()
  colorValues <- if (nUniqueSteps == 5) colors5 else colors8

  plt <-
    best |>
    mutate(deltaT = factor(sprintf("$2^{%d}$", log2(timeStep*nSteps))) |> fct_reorder(nSteps)) |>
    ggplot(aes(x = nObs, y = mean, color = deltaT)) +
    geom_line() + geom_point() +
    scale_x_log10() +
    scale_color_manual(values = colorValues) +
    xlab("$n$") + ylab(errorMetricLabelText) +
    labs(color = "$\\Delta\\!t$") +
    ggrepel::geom_label_repel(
      aes(label = deg),
      min.segment.length=0,
      force_pull = 1,
      force = 20,
      max.overlaps  = Inf,
      box.padding = 0,
      label.padding = 0.1,
      point.padding = 0,
      label.r = 0.0,
      label.size = 0.2,
    )
  set.seed(1)
  saveGgplotAsTikz(plt, file.path(plotDirPath, sprintf("%s_best_plot.tex", outFileLabel)), width=8, heigh=4)


  tableData <-
    best |>
    mutate(text = sprintf("%.1f (%d)", mean, deg)) |>
    mutate(nObsText = I(sprintf("$2^{%d} = %d$", log2(nObs), nObs))) |>
    select(nObs, nObsText, nSteps, text, mean) |>
    pivot_wider(names_from=nSteps, values_from=c(text, mean))

  bestTable <-
    tableData |>
    gt() |>
    cols_hide(c(starts_with("mean_"), nObs)) |>
    opt_row_striping(row_striping = TRUE) |>
    opt_vertical_padding(scale = 0) |>
    tab_options(table_body.hlines.style = "none") |>
    cols_move_to_start(nObsText) |>
    cols_label(
      nObsText = ""
    ) |>
    cols_label_with(
      starts_with("text_"),
      \(x) I(sprintf("$2^{%d}$", log2(as.integer(str_sub(x, start = 6))*timeStep)))
    ) |>
    tab_spanner(
      I("$\\stepsize$"),
      columns = starts_with("text_")
    ) |>
    tab_spanner(
      I("$n$"),
      nObsText
    ) |>
    data_color(
      columns = starts_with("mean_"),
      target_columns = starts_with("text_"),
      method = "numeric",
      palette = "viridis",
      domain = c(0, max(best$mean))
    )

  writeTableAsLatex(bestTable, file.path(tableDirPath, sprintf("%s_best_table.tex", outFileLabel)))


  label <- sprintf(
    "%s, %s%s%s, normalize %s, test %s, %s, noise level %.0e",
    experiment$system,
    experiment$systemPrecision,
    experiment$dataPrecision,
    experiment$methodPrecision,
    experiment$normalization,
    experiment$testMode,
    if (experiment$initalNoise) "noisy init.cond." else "noise-free init.cond.",
    experiment$noiseLevels
  )

  texTogetherLines <- c(
    r"(\clearpage)",
    sprintf(r"(\subsection{%s})", label),
    r"(\begin{figure}[ht!])",
    r"(\begin{center})",
    sprintf(r"(\includegraphics[width=\textwidth]{../img/%s_best_plot.pdf})", outFileLabel),
    r"(\end{center})",
    sprintf(r"(\caption{\textbf{Best Plot for %s}. See the beginning of \cref{app:sec:details} for a description.})", label),
    r"(\end{figure})",
    r"(\begin{table}[ht!])",
    sprintf(r"(\input{../tbl/%s_best_table.tex})", outFileLabel),
    sprintf(r"(\caption{\textbf{Best Table for %s}. See the beginning of \cref{app:sec:details} for a description.})", label),
    r"(\end{table})",
    r"(\begin{figure}[ht!])",
    r"(\begin{center})",
    sprintf( r"(\includegraphics[width=\textwidth]{../img/%s_all_plot.pdf})", outFileLabel),
    r"(\end{center})",
    sprintf(r"(\caption{\textbf{All Plot for %s}. See the beginning of \cref{app:sec:details} for a description.})", label),
    r"(\end{figure})",
    NULL
  )

  write_lines(texTogetherLines, file.path(sprintf("../tex/sec_app_details_%s.tex", outFileLabel)))
}



