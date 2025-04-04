
#' @title Set Cell Graphical Parameter Values
#'
#' @description Set the specific graphical parameters of the specified cell.
#' @param table gridExtra::tableGrob // The table to set the colour of.
#' @param x numeric // The x position (row) to look at.
#' @param y numeric // The y position (column) to look at.
#' @param ... List of arguments to pass to grid::gpar().
#' @param ignore_headings bool // Should the x and y coordinates be adjusted to
#' ignore the heading row and row-number coolumn?
#' @returns gridExtra::tableGrob // Return the input table with gpar adjustments applied.
set_cell_gpar_values <- function (table, x, y, ..., ignore_headings = T) {
  if (ignore_headings) {
    x = x + 1
    y = y + 1
  }
  index = table$layout$t == y &
    table$layout$l == x &
    table$layout$name == "core-bg"
  cell = which(index)
  table$grobs[cell][[1]][["gp"]] = grid::gpar(...)
  return(table)
}
