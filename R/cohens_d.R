
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
