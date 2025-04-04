
#' @title Add Test Objects
#'
#' @description Add a few test objects to the global environment for quick testing.
add_test_objects <- function() {
  list = list(a='a',A='A',b=0,B=1,c=T,C=F,d=NaN,D=1,e=Inf,E=10)
  list2env(list, envir=globalenv())
}
