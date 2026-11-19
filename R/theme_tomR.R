
#' @title GGPlot Theme tomR
#'
#' @description Add the official custom tomR ggplot theme to a ggplot object.
#' @import ggplot2
#' @export
theme_tomR <- function() {
  ggplot2::theme_grey() + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill="white"),
    panel.grid = ggplot2::element_line(color=NA),
    panel.border = ggplot2::element_rect(fill=NA, linewidth=12/22, color="black"),
    strip.background = ggplot2::element_rect(fill=NA, linewidth=12/22, color="black"),
    text = ggplot2::element_text(family="serif", size=12),
    axis.text = ggplot2::element_text(size=ggplot2::rel(0.9)),
    axis.title = ggplot2::element_text(size=ggplot2::rel(1)),
    legend.text = ggplot2::element_text(size=ggplot2::rel(0.9)),
    legend.title = ggplot2::element_text(size=ggplot2::rel(1)),
    plot.tag = ggplot2::element_text(size=ggplot2::rel(1)),
    plot.title = ggplot2::element_text(size=ggplot2::rel(1.2), hjust=0.5)
  )
}
