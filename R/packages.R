
#' @title Git It!
#'
#' @description Try to load a package you got from github, or, install it from github if you do not have it yet. If you do not have devtools the function will first install that. After doing all that, spin up the library in the current environment.
#' @param repo The name of the Github repository to load from, as if using install_github. If already installed will use the package portion to load the library.
#' @param quiet Stop the function from printing messages to your console.
git_it <- function(repo, quiet=F) {
  user = strsplit(repo,'/')[[1]][1]
  package = strsplit(repo,'/')[[1]][2]
  devtools.installed <- 'devtools' %in% rownames(utils::installed.packages())
  package.installed <- package %in% rownames(utils::installed.packages())
  # if devtools is not installed, get it
  if (!devtools.installed) {
    if(!quiet){message('\nYou do not have devtools installed, so I am doing that first.')}
    install.packages("devtools")
  }
  # if package has never been installed
  if (!package.installed) {
    if(!quiet){message('\n',package,' was never installed, so I am getting the latest version from github.\n')}
    devtools::install_github(repo)
  }
  # if package has been installed
  else {
    gh.url <- sprintf("https://raw.githubusercontent.com/%s/master/DESCRIPTION",repo)
    url.con <- url(gh.url)
    github.version <- as.character(read.dcf(url.con, fields="Version"))
    close(url.con)
    current.version <- installed.packages()[package,]['Version']
    # if package is out of date
    if (current.version != github.version) {
      if (!quiet) {message('\n',package,' is installed but out of date (',current.version,'), getting the new version (',github.version,').\n')}
      devtools::install_github(repo)
      current.version <- installed.packages()[package,]['Version']
    }
  }
  current.version <- installed.packages()[package,]['Version']
  # package must now be installed and up to date
  if(!quiet){message('I have gitten you ',package,' version ',current.version,'.\n')}
  library(package, character.only=T)
}

#' @title CRAN It!
#'
#' @description Try to load a package, and if you don't have it try to install it from CRAN instead.
#' @param package The name of the package to try and load.
cran_it <- function(package) {
  if(!package %in% rownames(utils::installed.packages())){
    utils::install.packages(package)
  }
  library(package, character.only=T)
}
