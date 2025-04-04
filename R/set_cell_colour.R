
#' @title Set Cell Colour
#'
#' @description Set the background colour of the specified cell.
#' @param table gridExtra::tableGrob // The table to set the colour of.
#' @param x numeric // The x position (row) to look at.
#' @param y numeric // The y position (column) to look at.
#' @param colour colour // The colour to set the background colour to.
#' @param ignore_headings bool // Should the x and y coordinates be adjusted to
#' ignore the heading row and row-number coolumn?
#' @returns gridExtra::tableGrob // Return the input table with the colour of the cell adjusted.
set_cell_colour <- function(table, x, y, colour, ignore_headings=T){
  if (ignore_headings) {x=x+1;y=y+1}
  index = table$layout$t==y&table$layout$l==x&table$layout$name=="core-bg"
  cell = which(index)
  table$grobs[cell][[1]][["gp"]] = grid::gpar(fill=colour, col='white', lwd=0.2)
  return(table)
}
