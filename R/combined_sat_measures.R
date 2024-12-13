
#' @title Inverse efficiency score
#'
#' @description Calculate IES.
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
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
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
lisas <- function(rt, pc){
  rt.sd = sqrt(((length(rt)-1)/length(rt))*stats::var(rt))
  pc.sd = sqrt(((length(pc)-1)/length(pc))*stats::var(pc))
  lisas.sd = rt.sd / pc.sd
  out = rt + lisas.sd * (1 - pc)
  return(out)
}

#' @title Balanced integration score
#'
#' @description Calculate BIS.
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
bis <- function(rt, pc){
  pc.sd = ssd(pc)
  rt.sd = ssd(rt)
  Z.pc = (pc - mean(pc)) / pc.sd
  Z.rt = (rt - mean(rt)) / rt.sd
  out  = Z.pc - Z.rt
  return(out)
}

#' @title Reward rate
#'
#' @description Calculate reward rate (Gold & Shadlen, 2002).
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
rr <- function(rt, pc) {
  out = pc / rt
  return(out)
}

#' @title Signed Residual Time
#'
#' @description Calculate signed residual time (Maris & van der Maas, 2012).
#' @param score Numeric Vector // A vector of scores representing correct or incorrect.
#' @param deadline Numeric Vector // A vector of deadline times, i.e. the time at which a response must have been given by.
#' @param rt Numeric Vector // A vector of response times.
srt <- function(score, deadline, rt) {
  out = ((2*score) - 1) * (deadline - rt)
  return(out)
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
