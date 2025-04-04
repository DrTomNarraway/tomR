
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
