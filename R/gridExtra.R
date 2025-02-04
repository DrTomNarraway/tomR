
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

#' @title Colour Blue to Orange.
#'
#' @description Apply a blue to orange gradient to values in this table showing
#' how positive or negative they are.
#' @param table The table to apply colouring to.
#' @return A gridExtra::tableGrob containing the values from the matrix.
colour_blue_to_orange <- function(table) {
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

#' @title Add Heading to Table.
#'
#' @description Append a heading to the top of a gridExtra::tableGrob table.
#' @param table gridExtra::tableGrob // The table to add the heading to.
#' @param heading string // The heading to add.
#' @param fontsize numeric // The size of the font of the heading.
#' @return The gridExtra::tableGrob table with the heading at the top.
add_heading_to_table <- function(table, heading='Heading', fontsize=18) {
  top = grid::textGrob(heading, gp=grid::gpar(fontsize=fontsize))
  out = gridExtra::arrangeGrob(table, top=top)
  return(out)
}
