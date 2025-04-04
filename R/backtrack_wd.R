
#' @title Backtrack Working Directory!
#'
#' @description Go through the current working directory and set a new WD from its path. Helpful for working on different devices with different paths to the same git repo.
#' @param target The name of the directory that will become the new working directory.
#' @param verbose Should stuff be printed to the console or not?
backtrack_wd <- function(target, verbose=F) {
  current.wd = getwd()
  split.wd = strsplit(current.wd, '/')[[1]]
  new.wd = ''
  for (i in 1:length(split.wd)) {
    new.wd = paste0(new.wd,split.wd[i],'/')
    if (split.wd[i] == target) {break}
  }
  setwd(new.wd)
  if (verbose) message('working directory is now ',getwd())
}
