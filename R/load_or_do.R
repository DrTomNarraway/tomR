
#' @title Load a File or do Something.
#'
#' @description Attempt to load file at this path, or save to this path if something was done.
#' @param path The path to the file you want to load, or save to if nothing is loaded.
#' @param do Function to perform if no file can be loaded.
#' @param quiet Suppress print and message commands. Default to false.
load_or_do <- function(path, do, quiet=F) {
  file_exists <- file.exists(path)
  if (file_exists) {
    if(!quiet){message('Loading file at \'',path,'\'.\n')}
    load(path, envir=globalenv())
  } else {
    if(!quiet){message('Doing...');print(do);message('')}
    do()
  }
}
