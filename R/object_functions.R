
#' @title Is Empty?
#'
#' @description Check if the passed object is "empty".
#' @param object Any R object.
#' @returns If an object of length 1 is passed: returns true if the object is False, "", " ", NULL, NaN, or Inf; and returns false otherwise.
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

#' @title Do All Levels Match?
#'
#' @description Check every level of the variables provided in object a and b are the same.
#' @param a data.frame // One of the objects to compare.
#' @param b data.frame // The other object to compare.
#' @param vars string or list of strings // The list of variables to observe.
#' @param ... string // Additional variables to observe.
#' @param verbose bool, default = F // Should we print stuff to the console?
#' @return A bool describing if every level of every variable is the same between a and b.
#' @note Because of the placement of ... you can pass an entire list to vars,
#' such as c('rt','pc','ID'), or you can write that list directly into the function
#' such as compare_levels_of_variables(a, b, 'rt', 'pc', 'ID') and the function will
#' work the exact same way.
do_all_levels_match <- function(a, b, vars, ..., verbose=F) {
  vars = c(vars, lapply(list(...), as.character))
  out = rep(NA, length(vars))
  names(out) = vars
  for (var in vars) {
    all_same = all(unique(a[[var]]) == unique(b[[var]]))
    if (all_same) out[var] = T else out[var] = F
  }
  if (verbose) print(out)
  return(all(out)==T)
}
