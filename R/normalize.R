
#' @title Normalize
#'
#' @description Re-scale a vector of any length to the range 0 to 1, aka, normalize it.
#' @param x The vector to normalize.
#' @return A vector of length x with each value normalized.
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}
