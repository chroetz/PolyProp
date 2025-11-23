if (getRversion() < "4.3") {
  stop(paste("R ≥ 4.3 is required. Detected:", getRversion()))
}

requiredPackages <- c(
  "tidyverse",
  "jsonlite",
  "arrow",
  "Rcpp",
  "RcppArmadillo",
  "sessioninfo",
  "rlang",
  "parallel",
  "ggrepel",
  "gt",
  "tikzDevice"
)

installIfMissing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(requiredPackages, installIfMissing))
