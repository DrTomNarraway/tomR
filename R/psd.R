
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
