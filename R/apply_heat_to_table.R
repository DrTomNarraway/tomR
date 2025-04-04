
#' @title Apply Heat to Table
#'
#' @description Heat up the tables of a gridExtra::tableGrob so that the largest
#' values glow red hot. In other words: convert it to a heatmap.
#' @param table The table to apply colouring to.
#' @return A gridExtra::tableGrob containing the values from the matrix.
#' @note This function will only add shading to positive values, you should use
#' colour_blue_to_orange for tables with positive and negative values.
apply_heat_to_table <- function(table) {
  reds = c("white","#FEE0D2","#FC9272","#CB181D","red")
  rows = nrow(table)-1
  cols = ncol(table)-1
  matrix = convert_table_to_matrix(table)
  max = as.numeric(max(matrix))
  for (y in 1:rows) {
    for (x in 1:cols) {
      content = get_cell_content(table, x, y)
      if (content=='-') fill = "white" else fill = reds[max(round((as.numeric(content)/max)*length(reds)),1)]
      table = set_cell_gpar_values(table, x, y, fill=fill, lty='blank')
    }
  }
  return(table)
}
