
#' @title Log A Index
#'
#' @description Estimate Log A Index (Dennis & Evans, 1996) for the given block.
#' LAI is a measure of ability regardless of the participants chosen speed-accuracy
#' threshold. LAI can be formally expressed as,
#' \deqn{LAI = \frac{-1}{RT - s} \cdot \log{\frac{A - logit(p)}{A}} }
#' where A is asymptotic accuracy, that is the greatest value logit(p) can take,
#' s is the minimum time necessary to respond above chance (i.e. non-decision time),
#' p is the probability of a correct response, and logit(p) is the log odds in favor
#' of a correct response.
#' @param rt Numeric vector of response times.
#' @param pc Numeric vector of proportion correct scores. The average PC score for
#' this block can be taken as p, the probabilty of a correct response.
#' @param s The minimum response time required for accuracy to be greater than chance.
#' This is equivilent to non-decision time, and as such the default value provided
#' is simply a common value for t0 plus a marginal amount.
#' @param A Asymptotic accuracy, aka, the maximum value logit(p) can ever take.
#' The default value provided (5.294) is marginally higher than logit(p) would be at an
#' accuracy score of 99.5\%.
#' @param max_p Decimal indicating the maximum possible probability of a correct response.
#' Dennis & Evans (1996) propose that this value should be arbitrarily set to 99.5\%.
#' @return A vector of scores of the same length as the passed vectors.
lai <- function(rt, pc, s=0.305, A=5.294, max_p=0.995) {
  if (length(rt) != length(pc)) stop('Passed vectors are not the same length.')
  p = min(mean(pc), max_p)
  logit_p = log(p / (1-p))
  lai = (-1 / (rt / s)) * log((A - logit_p) / A)
  return(lai)
}
