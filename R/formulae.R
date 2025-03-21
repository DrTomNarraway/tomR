
#' @title Sample Standard Deviation
#'
#' @description Calculate the sample standard deviation.
#' @param sample vector-like. The sample from which to calculate the standard deviation.
#' @param places numeric, default = 3. Value to round to. Pass in 0 or FALSE to not round.
ssd <- function(sample, places=3) {
  l      = length(sample)
  x      = sample
  x.mean = mean(sample)
  x.diff = x - x.mean
  x.sqrd = x.diff ^ 2
  x.sum  = sum(x.sqrd / l)
  out    = sqrt(x.sum)
  if (places > 0) out = round(out, places)
  return(out)
}

#' @title Percentage Standard Deviation
#'
#' @description Calculate the standard deviation of a percentage/mean value.
#' @param value numeric. Percentage from which the SD should be calculated.
#' @param dps numeric, default = 3 // Value to round to. Pass in 0 or FALSE to not round.
#' @param verbose bool, default = false // Should stuff be printed to the console or not.
psd <- function(value, dps=3, verbose=F){
  x    = value
  sqrd = value ^ 2
  xmsq = x - sqrd
  out  = sqrt( xmsq )
  if (dps > 0) out = round(out, dps)
  if (verbose) {
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
#' @param dps numeric, default = 3 // Value to round to. Pass in 0 or FALSE to not round.
#' @param ms_to_s bool, default = true // If true convert from ms to seconds.
#' @param verbose bool, default = false // Should stuff be printed to the console or not?
translate_drift <- function(d, dps=3, ms_to_s=T, verbose=F) {
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
  if (verbose) {
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
#' @param t numeric // The original threshold to be converted.
#' @param dps numeric, default = 3 // Value to round to. Pass in 0 or FALSE to not round.
#' @param ms_to_s bool, default = true // If true convert from ms to seconds.
#' @param verbose bool, default = false // Should stuff be printed to the console or not?
translate_threshold <- function(t, dps=3, ms_to_s=T, verbose=F) {
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
  if (verbose) {
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

#' @title Correlation from Standard Deviations
#'
#' @description Calculate the correlation of parameters given a known between- and within-subjects standard deviation.
#' @param subject.sd numeric // The known between-subjects standard deviation.
#' @param cond.sd numeric // The known within-subjects standard deviation.
#' @returns numeric // The correlation of parameters.
r_from_sds <- function(subject.sd, cond.sd) {return(subject.sd^2 / (subject.sd^2 + cond.sd^2))}

#' @title SD(Cond) from Correlation
#'
#' @description Calculate the condition-level standard deviation required to reach the desired correlation.
#' @param sd numeric // The known standard deviation, either between- or within-subjects.
#' @param r numeric // The desired correlation.
#' @returns numeric // The unknown standard deviation.
sd.cond_from_r <- function(sd, r) {return((sd * sqrt(r*(1-r))) / r)}

#' @title SD(Subject) from Correlation
#'
#' @description Calculate the subject-level standard deviation required to reach the desired correlation.
#' @param sd numeric // The known standard deviation, either between- or within-subjects.
#' @param r numeric // The desired correlation.
#' @returns numeric // The unknown standard deviation.
sd.subject_from_r <- function(sd, r) {return(return((sd * sqrt(r*(1-r))) / (1-r)))}

#' @title Z (Standardize Variable)
#'
#' @description Standardize a variable to allow easy comparison with other standardized variables.
#' @param sample numeric vector // The variable to standardize.
#' @param dps numeric, default = 3 // Value to round to. Pass in 0 or FALSE to not round.
#' @return numeric vector // The standardized vector.
Z <- function(sample, dps=0) {
  out = (sample-mean(sample))/stats::sd(sample)
  if (dps > 0) out = round(out, dps)
  return(out)
}

#' @title Cohen's D (Paired T-Test)
#'
#' @description Calculate Cohen's d using the t-statistic from the provided variables.
#' @param x Numeric vector of data from population x.
#' @param y Numeric vector of data from population y.
#' @return The calculated d parameter, automatically correct for Pearson's r as suggested by Cohen.
cohens_d = function(x, y) {
  t = unname(stats::t.test(x, y, paired=T)$statistic)
  N = unname(stats::t.test(x, y, paired=T)$parameter) + 1
  pearsons_r = stats::cor(x, y)
  d = (t/sqrt(N)) * sqrt(1-pearsons_r)
  return(d)
}

#' @title Calculate d' (d prime).
#'
#' @description Calculate the signal detection theory value d'.
#' This function automatically prevents H = 1 and FA = 0 errors by limiting h and fa based on n.
#' @param h Number or numeric-vector of hits.
#' @param fa Number  or numeric-vector of false alarms.
#' @param n Number  or numeric-vector of total trials, i.e. hits, false alarms, and misses.
#' @return A number or numeric-vector of d' values.
d_prime <- function(h, fa, n) {
  min_p = 1/n
  max_p = (n-1)/n
  H = pmax(pmin(h, max_p), min_p)
  FA = pmax(pmin(fa, max_p), min_p)
  dprime = stats::qnorm(H) - stats::qnorm(FA)
  return(dprime)
}
