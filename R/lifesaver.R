
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
