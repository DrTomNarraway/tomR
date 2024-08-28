
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

#' @title CRAN It!
#'
#' @description Try to load a package, and if you don't have it try to install it from CRAN instead.
#' @param package The name of the package to try and load.
cran_it <- function(package) {
  if(!package %in% rownames(installed.packages())){install.packages(package)}
  library(package, character.only=T)
}

#' @title Git It!
#'
#' @description Try to load a package you got from github, or, install it from github if you do not have it yet. If you cannot install from github it will also install devtools to allow it.
#' @param repo The name of the Github repository to load from, as if using install_github. If already installed will use the package portion to load the library.
git_it <- function(repo) {
  user = strsplit(repo,'/')[[1]][1]
  package = strsplit(repo,'/')[[1]][2]
  if(!'devtools' %in% rownames(installed.packages())){
    message('You do not have devtools installed, so I am doing that first.')
    install.packages("devtools")
  }
  if(!package %in% rownames(installed.packages())){
    message('You do not have ',package,' installed, so I am getting it from github.')
    devtools::install_github(repo)
  }
  library(package, character.only=T)
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
