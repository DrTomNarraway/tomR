
#' @title Inverse efficiency score
#'
#' @description Calculate IES for a given block. Each number should represent one
#' participants average across trials. IES can be expressed formally as,
#' \deqn{IES = \frac{\overline{RT}}{PC}}
#' @param rt Numeric vector of response times.
#' @param pc Numeric vector of proportion correct scores.
#' @return A vector of scores of the same length as the passed vectors.
ies <- function(rt, pc) {
  if (length(rt) != length(pc)) stop('Passed vectors are not the same length.')
  ies = rt / pc
  return(ies)
}
