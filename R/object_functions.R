
#' @title Is Empty?
#'
#' @description Check if the passed object is "empty".
#' @param object Any R object.
#' @value If an object of length 1 is passed: returns true if the object is False, "", " ", NULL, NaN, or Inf; and returns false otherwise.
#' If the passed object has length 2+: returns a vector of the same length with T/F for each element of the object.
is.empty <- function(object) {
  empties = c(F, '', ' ', NULL, NaN, Inf)
  if (length(object) == 1) {
    if (object %in% empties) return(TRUE)
    else return(FALSE)
  }
  else {
    out = object
    for (i in 1:length(object)) {
      if (object[i] %in% empties) out[i] = T
      else out[i] = F
    }
    return(out)
  }
}
