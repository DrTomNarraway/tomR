
#' @title CRAN It!
#'
#' @description Try to load a package, and if you don't have it try to install it from CRAN instead.
#' @param package The name of the package to try and load.
cran_it <- function(package) {
  if(!package %in% rownames(utils::installed.packages())) utils::install.packages(package)
  library(package, character.only=T)
}
