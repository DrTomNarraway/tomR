
#' @title Kill the Twin
#'
#' @description Perform a "kill the twin" procedure (Eriksen, 1988).
#' @param rt Numeric Vector // A vector of response times.
#' @param score Numeric Vector // A vector of scores representing correct or incorrect.
#' @returns Returns a data frame of survivors after performing "kill the twin".
ktt <- function(rt, score) {
  correct.rts = rt[score == 1]
  error.rts = rt[score == 0]
  n.errors = length(error.rts)
  to.remove = which(score == 0)
  for (e in 1:n.errors) {
    killer = error.rts[e]
    n = which.min(abs(correct.rts - killer))
    to.remove = c(to.remove, n)
  }
  out <- data.frame(
    rt = rt[-to.remove],
    score = score[-to.remove]
  )
  return(out)
}
