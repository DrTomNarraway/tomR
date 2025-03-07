
#' @title Return Rounded Seconds from Milliseconds
#'
#' @description Divide milliseconds by 1000 and then round to specified decimal places.
#' @param milliseconds Data to convert to seconds. Expects numeric.
#' @param dp Optional. Number of decimal places to round to. Defaults to 3.
#' @returns A data frame of rounded seconds of the same type as the input data.
return_rounded_seconds<-function(milliseconds, dp=3){
  return(round(milliseconds/1000, dp))
}

#' @title Meanify
#'
#' @description Group and summarise a data.frame into a meanified version.
#' @param data The data.frame to meanify.
#' @param ... The columns to group by.
#' @param rt.col The current name of the RT column. Will be renamed 'RT'.
#' @param score.col The current name of the score column. Will become the 'PC' (percentage correct) column.
meanify <- function(data, ..., rt.col='RT', score.col='Score') {
  out <- dplyr::group_by(.data=data, ...)
  colnames(out)[colnames(out)==rt.col] = 'RT'
  colnames(out)[colnames(out)==score.col] = 'Score'
  out <- dplyr::summarise(
    .data = out,
    .groups = 'keep',
    RT = mean(RT),
    PC = mean(Score)
  )
  return(out)
}

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

#' @title Normalize
#'
#' @description Re-scale a vector of any length to the range 0 to 1, aka, normalize it.
#' @param x The vector to normalize.
#' @return A vector of length x with each value normalized.
normalize <- function(x) {return((x-min(x))/(max(x)-min(x)))}
