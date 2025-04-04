
#' @title Colour Table Blue to Orange.
#'
#' @description Apply a blue to orange gradient to values in this table showing
#' how positive or negative they are.
#' @param table The table to apply colouring to.
#' @return A gridExtra::tableGrob containing the values from the matrix.
colour_table_blue_to_orange <- function(table) {
  colours = c("#D94701","#FD8D3C","#FDBE85","#FEEDDE","white","#EFF3FF","#BDD7E7","#6BAED6","#2171B5")
  rows = nrow(table)-1
  cols = ncol(table)-1
  matrix = convert_table_to_matrix(table)
  numbers = as.numeric(matrix[which(!is.na(suppressWarnings(as.numeric(matrix))))])
  max = max(numbers)
  min = min(numbers)
  for (y in 1:rows) {
    for (x in 1:cols) {
      content = get_cell_content(table, x, y)
      if (content=='-') fill = "white"
      if (content!='-') {
        number = as.numeric(content)
        rescaled = (number - min) / (max - min)
        index = round(rescaled * length(colours))
        mind = min(index, 9)
        maxd = max(index, 1)
        fill = colours[maxd]
      }
      table = set_cell_gpar_values(table, x, y, fill=fill, lty='blank')
    }
  }
  return(table)
}
