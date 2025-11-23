options(tidyverse.quiet = TRUE)
library(tidyverse)
source("common_paths.R")
source("common_io.R")
source("common_integrate.R")
source("common_defaults.R")

# default parameters
systemLabel <- "L96D5" # "L63" "TCSA"
duration <- 20
timeStep <- NULL
warmUpDuration <- NULL
solverPrecision <- "d"
defaults <- getDefaults()
swapAxis <- FALSE

# read command line arguments and set parameters
args <- commandArgs()
if ("--args" %in% args) eval(parse(text = paste(args[-(1:which(args == "--args"))], collapse=";")))

integrateSystem <- getIntegrateSystem(systemLabel, solverPrecision)
if (is.null(timeStep)) timeStep <- defaults[[systemLabel]]$timeStep
state0 <- defaults[[systemLabel]]$state0
n <- round(duration / timeStep) + 1
if (is.null(warmUpDuration)) {
  warmUpDuration <- duration * 0.1
}
nWarmUp <- round(warmUpDuration / timeStep)
dataAll <- integrateSystem(state0, timeStep, n + nWarmUp)
dataWarm <- dataAll[-seq_len(nWarmUp), ]


# project nD system to 2D
dim <- 2
y <- dataWarm[,-1]
d <- ncol(y)
n <- nrow(y)
center <- colMeans(y)
yCentered <- sweep(y, 2, center)
covmat <- stats::cov(yCentered)
eig <- eigen(covmat, symmetric=TRUE)
projectionMatrix <- eig$vectors[, seq_len(dim), drop=FALSE]
z <- yCentered %*% projectionMatrix


pltData <-
  tibble(
    time = dataWarm[, 1],
    state2D = if (swapAxis) z[, 2:1] else z[, 1:2],
  )

plt <-
  pltData |>
  ggplot(aes(x = .data$state2D[, 1], y = .data$state2D[, 2])) +
  geom_path() + coord_fixed(ratio = 1) + ggtitle(systemLabel) + xlab(NULL) + ylab(NULL)

saveGgplotAsPdf(plt, file.path(plotDirPath, sprintf("%s_attractor.pdf", systemLabel)), width=4, heigh=4)
