defaultsFilePath <- "./defaults.json"


replaceWithEval <- function(x, prefix, env = parent.frame()) {
  if (is.list(x)) {
    return(lapply(x, replaceWithEval, prefix = prefix, env = env))
  } else if (is.character(x)) {
    x <- sapply(x, \(element) {
      if (startsWith(element, prefix)) {
        exprText <- substr(element, nchar(prefix) + 1, nchar(element))
        exprs <- rlang::parse_exprs(exprText)
        res <- NULL
        for (expr in exprs) res <- rlang::eval_bare(expr, env = env)
        return(res)
      } else {
        return(element)
      }
    }, USE.NAMES = FALSE)
    return(x)
  } else {
    return(x)
  }
}


getDefaults <- function(env = parent.frame()) {
  defaultsRaw <- jsonlite::read_json(defaultsFilePath, simplifyVector=TRUE)
  replaceWithEval(defaultsRaw, "R>", env)
}
