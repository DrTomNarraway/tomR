
#' @title Create a Placeholder Matrix.
#'
#' @description Create a placeholder matrix to see how one will look.
#' @param len Length of the matrix sides.
#' @param diags list // What to place above and below the diagonal line.
#' @return A matrix containing the values provided in diags of size len.
create_placeholder_matrix <- function(len=3, diags=c('below', 'above')) {
  matrix = matrix(0, nrow=len, ncol=len)
  colnames(matrix) = rownames(matrix) = 1:len
  for (x in 1:len) {
    for  (y in 1:len) {
      if (x == y) matrix[y,x] = '-'
      if (x > y) matrix[y,x] = diags[2]
      if (x < y) matrix[y,x] = diags[1]
    }
  }
  return(matrix)
}

#' @title Create a Matrix of Random Values.
#'
#' @description Create a placeholder matrix with random values in it, where
#' those above the diagonal are positive and those below are negative.
#' @param len Length of the matrix sides.
#' @return A matrix containing random valuesof size len.
create_matrix_of_random_values <- function(len=3) {
  matrix = matrix(0, nrow=len, ncol=len)
  colnames(matrix) = rownames(matrix) = 1:len
  for (x in 1:len) {
    for  (y in 1:len) {
      if (x == y) matrix[y,x] = '-'
      if (x > y) matrix[y,x] = round(stats::runif(1), 3)
      if (x < y) matrix[y,x] = round(-1 * stats::runif(1), 3)
    }
  }
  return(matrix)
}
