
#' @title Progress (of this loop).
#'
#' @description cat the current progress of the loop as a percentage of x over y.
#' Always uses a \\r call to clear the line and replace the current percentage.
#' @param x Numeric variable indicating the current state of the loop.
#' @param y Numeric variable indicating the end of the loop.
#' @param dps Number of decimal places to round the percentage to.
#' @param colour The colour to print the percentage in. Must be available in \code{\link{ansi_colours}}.
progress <- function(x, y, dps=0, colour='white') {
  cat('\r',tomR::ansi_colours[colour],paste0(round((x/y)*100, dps),'%'),'\033[0m        ')
}

#' @title Nested Progress (of some loops).
#'
#' @description cat the current progress of a set of nested loops. Always uses a \\r call to clear the line and replace the current percentage.
#' @param ... Any number of pairs of the format c(current, max). Each listed pair will become one percentage display in the console.
#' @param dps Number of decimal places to round the percentage to.
nested_progress <- function (..., dps=0) {
  args = list(...)
  ps = rep(NA, length(args))
  for (i in 1:length(args))
    ps[i] = paste0(tomR::ansi_colours[i+1],stringr::str_pad(paste(round((args[[i]][1]/args[[i]][2])*100, dps)),3,pad="0"),'%\033[0m')
  cat("\r",ps,'        ')
}
