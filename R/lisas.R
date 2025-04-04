
#' @title Linear integrated speed-accuracy score
#'
#' @description Calculate LISAS for a given block. Each number should represent
#' one participants average across trials.
#' \deqn{LISAS = \overline{RT} + \frac{S_{RT}}{S_{PE}}\cdot PE}
#' @param rt Numeric vector of response times.
#' @param pc Numeric vector of proportion correct scores.
#' @return A vector of scores of the same length as the passed vectors.
#' @note LISAS is vulnerable to failure when PE = 0 because we both require the
#' standard deviation fo the PE distribution, and we multiply by PE. Because LISAS
#' is a measure of RT adjusted for errors, when there are no errors there is nothing
#' to adjust for and we can instead use the following formula,
#' \deqn{LISAS = \overline{RT}}
lisas <- function(rt, pc){
  if (length(rt) != length(pc)) stop('Passed vectors are not the same length.')
  if (stats::var(pc)!=0)
    lisas = rt + stats::sd(rt)/stats::sd(pc) * (1 - pc)
  else
    lisas = rt
  return(lisas)
}
