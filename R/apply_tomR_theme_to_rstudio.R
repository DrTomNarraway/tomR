
#' @title Apply custom tomR theme to RStudio.
#'
#' @description Install and apply the "official" tomR theme!
apply_tomR_theme_to_rstudio <- function() {
  rstudioapi::addTheme("https://raw.githubusercontent.com/DrTomNarraway/tomR/tomR.rstheme", apply=TRUE)
}
