
#' @title Remove All Variables
#'
#' @description Remove all objects from the global environment except constants. A constant is any object with an entirely uppercase name.
#' @param verbose List a bunch of stuff from the environment.
#' @param exceptions A list of variables to be excepted from the rm command.
remove_variables <- function(exceptions=c(), verbose=F) {
  all.objects = ls(globalenv())
  if (verbose) {message('found ',all.objects)}
  consts = c()
  for (object in all.objects) {
    if (object == toupper(object)) {consts = c(consts,object)}
  }
  to.remove = all.objects[!all.objects %in% consts & !all.objects %in% exceptions]
  if (verbose) {message('removing ',to.remove)}
  remove(list=to.remove, envir=globalenv(), inherits=FALSE)
}

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
