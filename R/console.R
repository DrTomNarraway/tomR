
#' @title Direct Message.
#'
#' @description Wrapper for printing to the console using message.
#' @param ... cohercable to string // Content to print to the console.
#' @param sep string // Separator to use between arguments.
#' @param lb bool // Should an empty line be printed after the message?
dm <- function(..., sep='', lb=TRUE) {
  args <- lapply(list(...), as.character)
  msg <- paste(args, collapse=sep)
  if (lb) {msg <- paste(msg,'\n')}
  message(msg)
}

#' @title Colour Printer.
#'
#' @description Print to the console in colour.
#' @param ... Cohered to String // Content to print to the console.
#' @param colour string // black, red, green, blue, pink, lavender, or yellow.
#' @param sep string // Separator to use between arguments.
#' @param lb bool // Should an empty line be printed after the message?
colour_printer <- function(..., colour='red', sep=' ', lb=TRUE){
  colour <- tomR::ansi_colours[colour]
  args <- lapply(list(...), as.character)
  msg <- paste(args, collapse=sep)
  if (lb) {msg <- paste(msg,'\n')}
  cat(paste0(colour, msg, "\033[0m\n"))
}

#' @title Return an Input From User.
#'
#' @description Give the user a set of options, each associated with a number, and get their input.
#' @param options The type that the response must be given in.
#' @param prompt The prompt for the user, printed as a message.
#' @return Returns one of the options provided to the options argument.
return_input <- function(options, prompt='Please choose from the following options:') {
  input <- NULL
  message(prompt)
  for (i in 1:length(options)){print(paste0(i,' = ',options[i]))}
  while (!input %in% 1:length(options) || !is.numeric(input)) {
    input <- as.numeric(readline('Choice: '))
    if(!input %in% 1:length(options)){print('That was not an option.')}
    if(!is.numeric(input)){print('That was not a number.')}
  }
  message('')
  return(options[input])
}

#' @title Pride Print.
#'
#' @description Print to the console in a cycle of colours, evoking the rainbow.
#' @param ... Cohered to String // Content to print to the console.
#' @param sep string // Separator to use between arguments.
#' @param lb bool // Should an empty line be printed after the message?
pride <- function(..., sep=' ', lb=TRUE){
  args <- lapply(list(...), as.character)
  colours <- tomR::ansi_colours[2:7]
  while (length(colours) < length(args)) {
    colours <- c(colours,colours)
  }
  msg <- ""
  for (i in 1:length(args)) {
    msg <- paste(msg, colours[i], args[i], "\033[0m", collapse=sep)
  }
  if (lb) {msg <- paste(msg,'\n')}
  cat(msg)
}

#' @title Progress (of this loop).
#'
#' @description cat print the current progress of the loop as a percentage of x over y.
#' Always uses a \\r call to clear the line and replace the current percentage.
#' @param x Numeric variable indicating the current state of the loop
#' @param y Numeric variable indicating the end of the loop.
#' @param dps Number of decimal places to round the percentage to.
#' @param colour The colour to print the percentage in. Must be available in \code{\link{ansi_colours}}.
progress <- function(x, y, dps=0, colour='white') {
  cat('\r',tomR::ansi_colours[colour],paste0(round((x/y)*100, dps),'%'),'\033[0m        ')
}

#' @title Spinner
#'
#' @encoding UTF-8
#' @description cat print a symbol to show the script is working. Always uses a \\r call to clear the line and replace the current percentage.
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
