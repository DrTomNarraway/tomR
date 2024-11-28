
#' @title Inverse efficiency score
#'
#' @description Calculate IES for a provided set of rts and pcs.
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
#' @param places numeric, default = 3. Value to round to. Pass in 0 or FALSE to not round.
ies <- function(rt, pc){ return(rt/pc) }

#' @title Rate correct score
#'
#' @description Calculate RCS for a provided set of rts and pcs.
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
#' @param places numeric, default = 3. Value to round to. Pass in 0 or FALSE to not round.
rcs <- function(rt, pc){ return(pc/sum(rt)) }

#' @title Linear integration score
#'
#' @description Calculate LISAS for a provided set of rts and pcs.
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
lisas <- function(rt, pc){
  rt.sd = sqrt(((length(rt)-1)/length(rt))*var(rt))
  pc.sd = sqrt(((length(pc)-1)/length(pc))*var(pc))
  lisas.sd = rt.sd / pc.sd
  out = rt + lisas.sd * (1 - pc)
  return(out)
}

#' @title Balanced integration score
#'
#' @description Calculate BIS for a provided set of rts and pcs.
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
#' @description Calculate reward rate (Gold & Shadlen, 2002) for a provided set of rts and pcs.
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
rr <- function(rt, pc){
  out = pc / rt
  return(out)
}

#' @title Signed Residual Time
#'
#' @description Calculate signed residual time (Maris & van der Maas, 2012) for a provided set of rts and pcs.
#' @param rt vector-like. The response time vector.
#' @param pc vector-like. The 'percent correct' vector.
srt <- function(rt, pc) {
  X = 0
  d = 0
  T.pi = 0
  out = (2 * X - 1) * (d - T.pi)
  return(out)
}
