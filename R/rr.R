
#' @title Reward rate (Trial)
#'
#' @description Calculate reward rate (Gold & Shadlen, 2002) for each provided trial.
#' @param rt Vector of response times, one per trial.
#' @param nc Vector indicating the outcome of the trial.
#' @return A vector of scores of the same length as the passed vectors.
rr <- function(rt, nc) {
  rr = nc / rt
  return(rr)
}

#' @title Reward rate (Block)
#'
#' @description Estimate reward rate (Gold & Shadlen, 2002) for the given averages.
#' @param rt Numeric vector of response times.
#' @param pc Numeric vector of proportion correct scores.
#' @param n Integer indicating the number of trials that make up this block.
#' @return A vector of scores of the same length as the passed vectors.
rr_block <- function(rt, pc, n) {
  rr = (pc * n) / (rt * n)
  return(rr)
}
