
#' @title Sample Standard Deviation
#'
#' @description Calculate the sample standard deviation.
#' @param sample vector-like. The sample from which to calculate the standard deviation.
#' @param places numeric, default = 3. Value to round to. Pass in 0 or FALSE to not round.
ssd <- function(sample, places=3) {
  l      = length(sample)
  x      = sample
  x.mean = mean(sample)
  x.diff = x - x.mean
  x.sqrd = x.diff ^ 2
  x.sum  = sum(x.sqrd / l)
  out    = sqrt(x.sum)
  if (places > 0) out = round(out, places)
  return(out)
}
