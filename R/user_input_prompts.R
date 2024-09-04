
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
  }
  message('')
  return(options[input])
}
