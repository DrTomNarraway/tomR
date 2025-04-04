
#' @title Balanced integration score
#'
#' @description Calculate BIS for a given block. Each number should represent one
#' Participants average across trials. BIS is able to integrate speed and accuracy
#' much more fairly than most other integrated measures because we first standardize
#' the distributions and then simply subtract one from the other. Here we subtract
#' RT from PC which is something akin to a measure of ability (above or below average).
#' BIS (of this form) can be formally expressed as,
#' \deqn{BIS = Z_{pc} - Z_{rt}}
#' @param rt Numeric vector of response times.
#' @param pc Numeric vector of proportion correct scores.
#' @return A vector of scores of the same length as the passed vectors.
#' @note In order to Z standardize a value it is necessary to divide by the standard
#' deviation of that values population. We run into a problem if there are no errors in a given block,
#' as this means there is no variance and thus the standard deviation equals 0.
#' When this is the case we use the alternate formula as follows,
#' \deqn{BIS = -1 \cdot Z_{rt}}
bis <- function(rt, pc){
  if (length(rt) != length(pc)) stop('Passed vectors are not the same length.')
  if (stats::var(pc) != 0)
    bis = Z(pc) - Z(rt)
  else
    bis = -1 * Z(rt)
  return(bis)
}
