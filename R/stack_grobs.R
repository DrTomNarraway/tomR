
#' @title Stack Graphical Objects
#'
#' @description Arrange grobs vertically and add a heading.
#' @param ... list // List of grobs (or grob-like) objects to arrange.
#' @param heading string // Optional heading to append to stacked grobs.
#' @param fontsize numeric // Size of font used for heading.
#' @returns grid // gridExtra grid of stacked grobs with heading.
stack_grobs <- function(..., heading='', fontsize=18) {
  top = grid::textGrob(paste(heading), gp=grid::gpar(fontsize=fontsize))
  grid = gridExtra::grid.arrange(..., ncol=1, top=top)
  return(grid)
}

#' @title Shelve Graphical Objects
#'
#' @description Arrange grobs horizontally and add a heading.
#' @param ... list // List of grobs (or grob-like) objects to arrange.
#' @param heading string // Optional heading to append to shelved grobs.
#' @param fontsize numeric // Size of font used for heading.
#' @returns grid // gridExtra grid of shelve grobs with heading.
shelve_grobs <- function(..., heading='', fontsize=24) {
  .length = length(list(...))
  top = grid::textGrob(paste(heading), gp=grid::gpar(fontsize=fontsize))
  grid = gridExtra::grid.arrange(..., ncol=.length, top=top)
  return(grid)
}
