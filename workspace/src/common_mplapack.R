mplapackPath <- normalizePath("~/MPLAPACK", winslash="/")

Sys.setenv(
  PKG_CXXFLAGS = paste0(
    "-I", mplapackPath, "/include ",
    "-I", mplapackPath, "/include/mplapack"
  ),
  PKG_LIBS = paste0(
    "-L", mplapackPath, "/lib ",
    "-lmplapack_mpfr -lmpblas_mpfr -lmpfr -lmpc -lgmp"
  ),
  PATH = paste(Sys.getenv("PATH"), normalizePath(file.path(mplapackPath, "bin")), sep=";"),
  LD_LIBRARY_PATH = paste0(mplapackPath, "/lib:", Sys.getenv("LD_LIBRARY_PATH"))
)
