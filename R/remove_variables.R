
#' @title Remove All Variables
#'
#' @description Remove all objects from the global environment except constants. A constant is any object with an entirely uppercase name.
remove_variables <- function() {
  list = ls(.GlobalEnv)[ls(.GlobalEnv)!=toupper(ls(.GlobalEnv))]
  remove(list=list, envir=.GlobalEnv, inherits=FALSE)
}
