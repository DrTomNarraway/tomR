
#' @title Inverse efficiency score
#'
#' @description Calculate IES.
#' @param rt Numeric vector of response times.
#' @param pc Numeric vector of proportion correct scores.
ies <- function(rt, pc) { return(rt/pc) }

#' @title Rate correct score
#'
#' @description Calculate RCS.
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
rcs <- function(rt, pc) { return(pc/sum(rt)) }

#' @title Linear integration score
#'
#' @description Calculate LISAS.
#' @param rt Numeric vector of response times.
#' @param pc Numeric vector of proportion correct scores.
lisas <- function(rt, pc){
  rt.sd = sqrt(((length(rt)-1)/length(rt))*stats::var(rt))
  pc.sd = sqrt(((length(pc)-1)/length(pc))*stats::var(pc))
  lisas.sd = rt.sd / pc.sd
  out = rt + lisas.sd * (1 - pc)
  return(out)
}

#' @title Balanced integration score
#'
#' @description Calculate BIS () for a given block.
#' @param rt Numeric vector of response times.
#' @param pc Numeric vector of proportion correct scores.
bis <- function(rt, pc){
  pc.sd = ssd(pc)
  rt.sd = ssd(rt)
  Z.pc = (pc - mean(pc)) / pc.sd
  Z.rt = (rt - mean(rt)) / rt.sd
  out  = Z.pc - Z.rt
  return(out)
}

#' @title Reward rate (Trial)
#'
#' @description Calculate reward rate (Gold & Shadlen, 2002) for each provided trial.
#' @param rt Vector of response times, one per trial.
#' @param nc Vector indicating the outcome of the trial.
#' @return Vector of length(rt) indicating the reward rate.
rr <- function(rt, nc) {
  rr = nc / rt
  return(rr)
}

#' @title Reward rate (Block)
#'
#' @description Estimate reward rate (Gold & Shadlen, 2002) for the given averages.
#' @param rt Mean response time.
#' @param pc Proportion of correct responses.
#' @param n The number of trials that make up this block.
#' @return Vector of length(rt) indicating the reward rate.
rr_block <- function(rt, pc, n) {
  rr = (pc * n) / (rt * n)
  return(rr)
}

#' @title Signed Residual Time (Trial)
#'
#' @description Calculate signed residual time (Maris & van der Maas, 2012) for each provided trial.
#' @param rt Numeric vector of response times.
#' @param x Numeric Vector of scores, should be coded correct = 1, incorrect = 0.
#' @param deadline Number or numeric vector of length(rt) indicating the trial deadline.
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
#' @param n Intiger indicating the number of trials that make up this block.
#' @param deadline Number indicating the mean trial deadline.
srt_block <- function(rt, pc, n, deadline) {
  x = pc * n # estimated sum of scores
  srt = ((2*x) - n) * (deadline - rt)
  return(srt)
}

#' @title Kill the Twin
#'
#' @description Perform a "kill the twin" procedure (Eriksen, 1988).
#' @param rt Numeric Vector // A vector of response times.
#' @param score Numeric Vector // A vector of scores representing correct or incorrect.
#' @returns Returns a data frame of survivors after performing "kill the twin".
ktt <- function(rt, score) {
  correct.rts = rt[score == 1]
  error.rts = rt[score == 0]
  n.errors = length(error.rts)
  to.remove = which(score == 0)
  for (e in 1:n.errors) {
    killer = error.rts[e]
    n = which.min(abs(correct.rts - killer))
    to.remove = c(to.remove, n)
  }
  out <- data.frame(
    rt = rt[-to.remove],
    score = score[-to.remove]
  )
  return(out)
}

#' @title Log A Index (Trial)
#'
#' @description Calculate Log A Index (Dennis & Evans, 1996) for each provided trial.
#' @param rt Numeric vector of response times, one per trial.
#' @param p The probability of a correct response.
#' @param s The minimum response time required for accuracy to be greater than chance.
#' @param A Asymptotic accuracy, aka, the maximum value logit(p) can ever take.
lai <- function(rt, p=0.5, s=0.2, A=log(999)) {
  logit_p = log(p / (1 - p))
  lai = (-1 * (rt / s)) * log((A - logit_p) / A)
  return(lai)
}

#' @title Log A Index (Block)
#'
#' @description Estimate Log A Index (Dennis & Evans, 1996) for the given averages.
#' @param rt Mean response time of the given block.
#' @param pc Proportion correct for the given block.
#' @param s The minimum response time required for accuracy to be greater than chance.
#' @param A Asymptotic accuracy, aka, the maximum value logit(p) can ever take.
lai_block <- function(rt, pc, s=0.2, A=log(999)) {
  logit_p = log(pc / (1-pc))
  lai = (-1 * (rt / s)) * log((A - logit_p) / A)
  return(lai)
}
