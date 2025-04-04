
#' @title Convert Matrix to Table
#'
#' @description Convert a matrix to a gridExtra::tableGrob.
#' @param matrix The matrix to convert.
#' @return A gridExtra::tableGrob containing the values from the matrix.
#' @note # returns the table with an aesthetic similar to the ggplot theme_classic
#' aesthetics. You cannot control or change those details here.
convert_matrix_to_table <- function(matrix) {
  .cols = ncol(matrix)
  .rows = nrow(matrix)
  .core = list(fg_params=list(fontface=3))
  .head = list(fg_params=list(fontface=2))
  .hline = grid::segmentsGrob(x0=grid::unit(0,"npc"),y0=grid::unit(0,"npc"),x1=grid::unit(1,"npc"),y1=grid::unit(0,"npc"),gp=grid::gpar(lwd=2.0))
  .vline = grid::segmentsGrob(x0=grid::unit(0,"npc"),y0=grid::unit(0,"npc"),x1=grid::unit(0,"npc"),y1=grid::unit(1,"npc"),gp=grid::gpar(lwd=2.0))
  theme = gridExtra::ttheme_minimal(core=.core, colhead=.head, rowhead=.head)
  table = gridExtra::tableGrob(matrix, theme=theme)
  table = gtable::gtable_add_grob(table, .hline, t=1, b=1, l=2, r=.rows+1)
  table = gtable::gtable_add_grob(table, .vline, t=2, b=.cols+1, l=2, r=2)
  return(table)
}

#' @title Convert Table to Matrix
#'
#' @description Convert a gridExtra::tableGrob to a matrix.
#' @param table The gridExtra::tableGrob to convert.
#' @return A matrix of the cell values from the provided table.
convert_table_to_matrix <- function(table) {
  rows = nrow(table)-1
  cols = ncol(table)-1
  matrix = matrix(NA, rows, cols)
  rownames(matrix) = colnames(matrix) = 1:cols
  for (y in 1:rows) {
    for (x in 1:cols) {
      matrix[y,x] = get_cell_content(table, x, y)
    }
  }
  return(matrix)
}
