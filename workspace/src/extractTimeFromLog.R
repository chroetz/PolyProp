options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")

filePaths <- list.files(logDirPath, pattern="^(L63|TCSA|L96D\\d+).*\\.out$", full.names=TRUE)

data <-
  lapply(
    filePaths,
    \(filePath) {
      lines <- read_lines(filePath, progress = FALSE)
      startIdx <- str_which(lines, "^n: \\d+, step: \\d+$")
      breakIdx <- c(startIdx, length(lines))
      dataOfFile <-
        lapply(
          seq_along(startIdx),
          \(i) {
            stringsHead <- str_match(lines[startIdx[i]], "^n: (\\d+), step: (\\d+)$")
            strings <- str_match(lines[(breakIdx[i]+1):(breakIdx[i+1]-1)], "^nDeg: (\\d+)... took (\\d+.\\d+)s.$")
            tibble(
              n = as.integer(stringsHead[2]),
              step =  as.integer(stringsHead[3]),
              nDeg = as.integer(strings[,2]),
              time = as.double(strings[,3])
            )
          }
        ) |>
        bind_rows()
      fileName <- basename(filePath)
      stringsFileName <- str_match(fileName, "^(L63|TCSA|L96D\\d+)_([^_]+)_.*\\.out$")
      dataOfFile |>
        mutate(system = stringsFileName[2], label = stringsFileName[3])
    }
  ) |>
  bind_rows()

dataSummary <-
  data |>
  drop_na() |>
  summarize(meanTime = mean(time), nReps = n(), .by = c(label, n, step, nDeg, system))

write_csv(dataSummary, file.path(evalDirPath, "computeTime.csv"))
