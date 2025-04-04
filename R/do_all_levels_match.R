
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
