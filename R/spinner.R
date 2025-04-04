
#' @title Spinner
#'
#' @encoding UTF-8
#' @description cat a symbol to show the script is working. Always uses a \\r call to clear the line and replace the current percentage.
#' @param i Numeric variable indicating the current state of the loop.
#' @param set Symbol set to use for spinner. The following sets are currently available:
#' \itemize{
#'  \item  circle:   `r "\U25D0"`, `r "\U25D3"`, `r "\U25D1"`, `r "\U25D2"`
#'  \item  corner:   `r "\U2599"`, `r "\U259B"`, `r "\U259C"`, `r "\U259F"`
#'  \item  quadrant: `r "\U2596"`, `r "\U2598"`, `r "\U259D"`, `r "\U2597"`
#' }
#' @param colour The colour to print the spinner in. Must be available in \code{\link{ansi_colours}}.
#' @md
spinner <- function(i, set='circle', colour='white') {
  symbols = list(
    'circle' = c("\U25D0","\U25D3","\U25D1","\U25D2"),
    'corner' = c("\U2599","\U259B","\U259C","\U259F"),
    'quadrant' = c("\U2596","\U259D","\U2598","\U2597")
  )
  cat('\r',tomR::ansi_colours[colour],symbols[[set]][(i%%length(symbols[[set]]))+1],'\033[0m        ')
}
