
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
