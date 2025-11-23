openblasPath <- normalizePath("~/OpenBLAS", winslash="/")

Sys.setenv(
  PKG_CXXFLAGS = paste0(
    "-I", openblasPath, "/include ",
    "-I", openblasPath, "/include/mplapack"
  ),
  PKG_LIBS = paste0(
    "-L", openblasPath, "/lib ",
    "-lopenblas"
  ),
  PATH = paste(Sys.getenv("PATH"), normalizePath(file.path(openblasPath, "bin")), sep=";"),
  LD_LIBRARY_PATH = paste0(openblasPath, "/lib:", Sys.getenv("LD_LIBRARY_PATH"))
)
