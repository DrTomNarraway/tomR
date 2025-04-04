
#' @title Signed Residual Time (Trial)
#'
#' @description Calculate signed residual time (Maris & van der Maas, 2012) for each provided trial.
#' @param rt Numeric vector of response times.
#' @param x Numeric Vector of scores, should be coded correct = 1, incorrect = 0.
#' @param deadline Number or numeric vector of length(rt) indicating the trial deadline.
#' @return A vector of scores of the same length as the passed vectors.
srt <- function(rt, x, deadline) {
  srt = ((2*x) - 1) * (deadline - rt)
  return(srt)
}

#' @title Signed Residual Time (Block)
#'
#' @description Estimate signed residual time (Maris & van der Maas, 2012) for the given averages.
#' Useful when data is already aggregates across trials, but less accurate.
#' @param rt Float indicating the mean response time.
#' @param pc Float indicating the proportion of correct responses.
#' @param n Integer indicating the number of trials that make up this block.
#' @param deadline Number indicating the mean trial deadline.
#' @return A vector of scores of the same length as the passed vectors.
srt_block <- function(rt, pc, n, deadline) {
  x = pc * n # estimated sum of scores
  srt = ((2*x) - n) * (deadline - rt)
  return(srt)
}
