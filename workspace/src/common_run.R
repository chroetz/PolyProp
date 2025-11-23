runCmdsLocal <- function(cmds, parallel = FALSE) {
  if (length(cmds) > 1 && parallel && require("parallel") && detectCores() > 2) {
    cat(sprintf("Run %d commands locally in parallel.\n", length(cmds)))
    cat(paste(cmds, collapse="\n"), "\n")
    numCores <- detectCores() - 1
    numCores <- pmin(numCores, length(cmds))
    cl <- makeCluster(numCores)
    clusterApplyLB(cl, cmds, system)
    stopCluster(cl)
  } else {
    cat(sprintf("Run %d commands locally in sequence.\n", length(cmds)))
    for (cmd in cmds) {
      cat(cmd, "\n")
      system(cmd)
    }
  }
}


runCmdsSlurm <- function(cmds, label, qos="short", timeInMin=1440, nCpus=1, memInGb=NULL) {

  cmds <- gsub("\"", "\\\\\"", cmds) # escape the symbol " in commands

  slurmCmds <- paste(
    "sbatch",
    sprintf("--qos=%s", qos),
    sprintf("--job-name=%s", label),
    sprintf("--output=_log/%s_%%j.out", label),
    sprintf("--error=_log/%s_%%j.err", label),
    sprintf("--time=%d", timeInMin),
    sprintf("--cpus-per-task=%d", nCpus),
    if (!is.null(memInGb)) sprintf("--mem=%dG", memInGb),
    paste0("--wrap=\"", cmds, "\"")
  )

  dir.create("_log", showWarnings=FALSE)

  cat(sprintf("Run %d commands using slurm.\n", length(slurmCmds)))
  
  for (slurmCmd in slurmCmds) {
    cat(slurmCmd, "\n")
    system(slurmCmd)
  }
}


runCmds <- function(cmds, runMode, label, ...) {
  switch(
    runMode,
    s = { # serial
      runCmdsLocal(cmds, parallel = FALSE)
    },
    p = { # parallel
      runCmdsLocal(cmds, parallel = TRUE)
    },
    c = { # cluster with slurm
      runCmdsSlurm(cmds, label, ...)
    }
  )
}


makeCmd <- function(scriptFile, args) {
  # only use single quotes in args
  paste(
    "Rscript",
    scriptFile,
    paste0("\"", args, "\"", collapse=" ")
  )
}
