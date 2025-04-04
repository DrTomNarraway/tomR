
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
