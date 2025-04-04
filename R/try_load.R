
#' @title Try and Load a File.
#'
#' @description Attempt to load a file, or stop and warn if the path is incorrect.
#' @param path The path to the file you want to load.
#' @param error_message Message to display if an error occurs.
try_load <- function(path, error_message='File not found.') {
  if (!file.exists(path)) {
    message('')
    print(error_message)
  }
  load(path, envir=globalenv())
}
