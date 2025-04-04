
#' @title Get Cell Content
#'
#' @description Find the content of the specified cell of a table.
#' @param table gridExtra::tableGrob // The table to search the contents of.
#' @param x numeric // The x position (row) to look at.
#' @param y numeric // The y position (column) to look at.
#' @param ignore_headings bool // Should the x and y coordinates be adjusted to
#' ignore the heading row and row-number coolumn?
get_cell_content <- function(table, x, y, ignore_headings=T) {
  if (ignore_headings) {x=x+1;y=y+1}
  cell = which(table$layout$t==y & table$layout$l==x)
  out = table$grobs[cell][[1]]['label'][[1]]
  return(out)
}
