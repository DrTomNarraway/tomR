
#' @title Rate correct score
#'
#' @description Calculate RCS for a given block. Each number should represent one
#' participants average across trials. RCS can be expressed formally as,
#' \deqn{RCS = \frac{NC}{\sum{RT}}}
#' @param rt Numeric vector of response times.
#' @param pc Numeric vector of proportion correct scores.
#' @param n Integer indicating the number of trials that make up this block.
#' @return A vector of scores of the same length as the passed vectors.
rcs <- function(rt, pc, n) {
  if (length(rt) != length(pc)) stop('Passed vectors are not the same length.')
  rcs = (pc*n) / sum(rt)
  return(rcs)
}
