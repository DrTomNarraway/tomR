
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
