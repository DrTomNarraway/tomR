#' @title Try and Load a File.
#'
#' @description Attempt to load a file, or stop and warn if the path is incorrect.
#' @param path The path to the file you want to load.
#' @param error_message Message to display if an error occurs.
try_load <- function(path, error_message) {
  if (!file.exists(path)) {
    message('')
    print(error_message)
  }
  load(path, envir=globalenv())
}

#' @title Ask which file to load from a Path.
#'
#' @description Look at all of the files in a directory and be prompted to choose one of them to load.
#' @param dir The path to the directory where your files are located.
ask_from_path <- function(dir) {
  last_char <- substr(dir, nchar(dir), nchar(dir))
  if (!last_char %in% c('\\','/')) {dir <- paste0(dir,'\\')}
  files <- list.files(dir)
  path_to_file <- ''
  if (length(files) > 1) {
    message('')
    for (f in 1:length(files)) {message(f,' : ',files[f])}
    message('')
    input <- NULL
    while (!is.numeric(input)) {input <- as.numeric(readline('Please use a number to choose one of the listed files: '))}
    path_to_file <- paste0(dir,files[input])
  }
  else {
    message('')
    message('Only one file found, so I am just going to load that one.')
    message('')
    path_to_file <- paste0(dir,files[1])
  }
  load(path_to_file, envir=globalenv())
}
