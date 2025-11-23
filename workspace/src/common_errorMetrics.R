validPredictionTime <- function(err, time, thresh, err0, lyapunov) {
  lost <- err >= thresh * err0
  lost[is.na(lost)] <- TRUE
  idxFirstAboveThresh <- which(lost)[1]
  if (is.na(idxFirstAboveThresh)) {
    vpt <- max(time)
  } else if (idxFirstAboveThresh == 1) {
    vpt <- 0
  } else {
    vpt <- time[idxFirstAboveThresh-1]
  }
  vpt * lyapunov
}

cumulativeValidityScore <- function(err, time, thresh, err0, lyapunov) {
  x <- err / err0
  x[is.na(x)] <- thresh
  x <- pmin(thresh, cummax(x))
  cvs <- mean(1-x) * max(time) * lyapunov 
  return(cvs)
}

oneLyapunovTimeNrmse <- function(err, time, err0, lyapunov) {
  x <- err / err0
  mean(x[time * lyapunov <= 1])
}

numberOfPerfectPredictions <- function(err) {
  sum(err == 0, na.rm=TRUE)
}
