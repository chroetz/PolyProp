getIntegrateSystem <- function(systemLabel, solverPrecision) {

  unknownSolverErrorMessage <- paste0(
    "Unknown solverPrecision ", solverPrecision, ". Use s (single, 32bit), d (double, 64bit) or m (multi, 512bit).")

  if (stringr::str_detect(systemLabel, "^L96(D(\\d+))?$")) {

    switch(
      solverPrecision,
      s = {Rcpp::sourceCpp("integrate_L96_32bit.cpp");return(integrate_L96_32bit)},
      d = {Rcpp::sourceCpp("integrate_L96_64bit.cpp");return(integrate_L96_64bit)},
      m = stop("For L96, only solverPrecision = 'd' and 's' are implemented."),
      stop(unknownSolverErrorMessage)
    )

  } else {

    switch(
      systemLabel,
      L63 = switch(
        solverPrecision,
        s = {Rcpp::sourceCpp("integrate_L63_32bit.cpp");return(integrate_L63_32bit)},
        d = {Rcpp::sourceCpp("integrate_L63_64bit.cpp");return(integrate_L63_64bit)},
        m = {
          source("common_mplapack.R")
          Rcpp::sourceCpp("integrate_L63_512bit.cpp")
          return(integrate_L63_512bit)
        },
        y = {
          source("common_mplapack.R")
          Rcpp::sourceCpp("integrate_L63_y_512bit.cpp")
          return(integrate_L63_y_512bit)
        },
        stop(unknownSolverErrorMessage)
      ),
      TCSA = switch(
        solverPrecision,
        s = {Rcpp::sourceCpp("integrate_TCSA_32bit.cpp");return(integrate_TCSA_32bit)},
        d = {Rcpp::sourceCpp("integrate_TCSA_64bit.cpp");return(integrate_TCSA_64bit)},
        m = {
          source("common_mplapack.R")
          Rcpp::sourceCpp("integrate_TCSA_512bit.cpp")
          return(integrate_TCSA_512bit)
        },
        stop(unknownSolverErrorMessage)
      ),
      stop(
        "Unknown systemLabel ", systemLabel,
        ". Use L63 (Lorenz 63) or TCSA (Thomas' Cyclically Symmetric Atttactor) or L96 (Lorenz 96).")
    )
  }
}
