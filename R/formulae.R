
#' @title Sample Standard Deviation
#'
#' @description Calculate the sample standard deviation.
#' @param sample vector-like. The sample from which to calculate the standard deviation.
#' @param dps numeric, default = 3. Value to round to. Pass in 0 or FALSE to not round.
#' @param quiet bool, default = false. If quiet do not print stuff to console.
ssd <- function(sample, dps=3, quiet=F){
  l      = length(sample)
  x      = sample
  x.mean = mean(sample)
  x.diff = x - x.mean
  x.sqrd = x.diff ^ 2
  x.sum  = sum(x.sqrd / l)
  out    = sqrt(x.sum)
  if (dps > 0) out = round(out, dps)
  if (!quiet) {
    pride(
      paste0('sample mean = ',x),
      paste0('n = ',n),
      paste0('out = ',out)
    )
    message('')
  }
  return(out)
}

#' @title Percentage Standard Deviation
#'
#' @description Calculate the standard deviation of a percentage/mean value.
#' @param value numeric. Percentage from which the SD should be calculated.
#' @param dps numeric, default = 3. Value to round to. Pass in 0 or FALSE to not round.
#' @param quiet bool, default = false. If quiet do not print stuff to console.
psd <- function(value, dps=3, quiet=F){
  x    = value
  sqrd = value ^ 2
  xmsq = x - sqrd
  out  = sqrt( xmsq )
  if (dps > 0) out = round(out, dps)
  if (!quiet) {
    pride(
      paste0('value = ',value),
      paste0('value ^ 2 = ',sqrd),
      paste0('x - value ^ 2 = ',xmsq),
      paste0('psd = ',out)
    )
    message('')
  }
  return(out)
}

#' @title Translate Drift Rate
#'
#' @description Translate drift rate with sigma = 4 in ms to sigma = 1 in seconds.
#' @param d numeric. The original drift rate to be converted.
#' @param ms_to_s bool, default = true. If true convert from ms to seconds.
#' @param dps numeric, default = 3. Value to round to. Pass in 0 or FALSE to not round.
#' @param quiet bool, default = false. If quiet do not print stuff to console.
translate_drift <- function(d, ms_to_s=T, dps=3, quiet=F) {
  if (ms_to_s) {
    drift = d / 4
    time = 1000 / sqrt(1000)
    out = drift * time
  }
  else {
    drift = d * 4
    time = sqrt(1000)
    out = drift / time
  }
  if (dps > 0) out = round(out, dps)
  if (!quiet) {
    pride(
      paste0('d = ',d),
      paste0('ms to seconds = ',ms_to_s),
      paste0('drift = ',drift),
      paste0('time = ',time),
      paste0('translated drift = ',out)
    )
    message('')
  }
  return(out)
}

#' @title Translate Threshold
#'
#' @description Translate threshold with sigma = 4 in ms to sigma = 1 in seconds.
#' @param t numeric. The original threshold to be converted.
#' @param ms_to_s bool, default = true. If true convert from ms to seconds.
#' @param dps numeric, default = 3. Value to round to. Pass in 0 or FALSE to not round.
#' @param quiet bool, default = false. If quiet do not print stuff to console.
translate_threshold <- function(t, ms_to_s=T, dps=3, quiet=F) {
  if (ms_to_s) {
    threshold <- t / 4
    time = sqrt(1000)
    out = threshold / time
  }
  else {
    threshold <- t * 4
    time = 1000 / sqrt(1000)
    out = threshold * time
  }
  if (dps > 0) out = round(out, dps)
  if (!quiet) {
    pride(
      paste0('t = ',t),
      paste0('ms to seconds = ',ms_to_s),
      paste0('threshold = ',threshold),
      paste0('time = ',time),
      paste0('translated threshold = ',out)
    )
    message('')
  }
  return(out)
}
