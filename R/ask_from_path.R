
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
    for (f in 1:length(files)) {message(f,' : ',files[f])}
    input <- tomR::return_input(files)
    path_to_file <- paste0(dir,input)
  }
  else {
    message('Only one file found, so I am just going to load that one.\n')
    path_to_file <- paste0(dir,files[1])
  }
  load(path_to_file, envir=globalenv())
}
