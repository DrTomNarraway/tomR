
#' @title Create Significance Matrix
#'
#' @description Create a matrix counting the number of positive t-tests.
#' @param data data.frame // The data.
#' @param dim string // The variable to form the matrix from, i.e. the 'sides'.
#' @param diag string // The variable to vary above and below the diagonal line.
#' @param over string // The variable to loop over while counting number of significant tests.
#' @param var string // The variable to perform t-tests on.
#' @param alpha numeric, default = 0.05 // The alpha value to use for the t-tests.
create_sig_matrix <- function(data, dim, diag, over, var, alpha=0.05) {
  if (is.null(data[[dim]])) stop('Matrix dimension not found in data.frame provided.')
  if (is.null(data[[diag]])) stop('Diagonal dimension not found in data.frame provided.')
  if (is.null(data[[over]])) stop('Variable for looping over not found in data.frame provided.')
  if (is.null(data[[var]])) stop('Variable for t.tests not found in data.frame provided.')
  overs = unique(data[[over]])
  n.overs = as.numeric(length(overs))
  dims = unique(data[[dim]])
  len = length(dims)
  matrix = matrix(0,nrow=len,ncol=len)
  colnames(matrix) = rownames(matrix) = dims
  diags = unique(data[[diag]])
  if (length(diags)!=2) stop('Incorrect number of diagonal variables.')
  for (x in 1:len) {
    dim1 = dims[x]
    for  (y in 1:len) {
      if (x == y) {
        matrix[y,x] = '-'
        next
      }
      if (x > y) dg = diags[2]
      if (x < y) dg = diags[1]
      dim2 = dims[y]
      sigs = 0
      for (ovr in 1:n.overs) {
        rows = data[[dim]]%in%c(dim1,dim2) & data[[diag]]==dg & data[[over]]==ovr
        t.test = t.test(data[[var]][rows] ~ data[[dim]][rows], data=data[rows, ])
        if (t.test$p.value <= alpha) sigs = sigs + 1
      }
      matrix[y,x] = sigs
    }
  }
  return(matrix)
}
