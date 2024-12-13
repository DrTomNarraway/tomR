
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

#' @title Lifesaver
#'
#' @description Wrapper to prevent accidentally saving over a file that already exists.
#' @param .object The R object to try and save.
#' @param file The file path you want to save to.
#' @return True if file was saved, false otherwise.
lifesaver <- function(.object, file) {
  if (file.exists(file)) {
    print('A file already exists at the path. \nAre you sure you want to save?')
    options = c('SAVE','',' ')
    input = 'INPUT'
    while (!input %in% options) {input = readline('Type SAVE into the console to save, \nor leave blank to exit without saving: ')}
    if (input == 'SAVE') {
      save(.object, file=file)
      return(T)
    }
    else {
      print('Proceeding without saving.')
      return(F)
    }
  }
  else {
    save(.object, file=file)
    return(T)
  }
}
