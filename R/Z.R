
#' @title Z (Standardize Variable)
#'
#' @description Standardize a variable to allow easy comparison with other standardized variables.
#' @param sample numeric vector // The variable to standardize.
#' @param dps numeric, default = 3 // Value to round to. Pass in 0 or FALSE to not round.
#' @return numeric vector // The standardized vector.
Z <- function(sample, dps=0) {
  out = (sample-mean(sample))/stats::sd(sample)
  if (dps > 0) out = round(out, dps)
  return(out)
}
