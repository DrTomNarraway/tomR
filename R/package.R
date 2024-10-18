
#' @title Build tomR.
#'
#' @description Build this package and iterate its version.
#' @param version major, minor, patch, debug // Type of increment to perform.
build_tomR <- function(version='debug') {
  checked <- devtools::check()
  if (length(checked$errors) > 0 | length(checked$warnings) > 0 ) {
    proceed = NA
    colour_printer(length(checked$errors),' errors found.')
    colour_printer(length(checked$errors),' warnings found.', colour='yellow')
    prompt <- paste0('Given these warnings, errors, or notes: proceed? ')
    while (!proceed %in% c(T, F)) {
      proceed <- tomR::return_input(c(T, F), prompt=prompt)
    }
    if (!proceed) {return(checked)}
  }
  devtools::document()
  if (version != 'debug') { usethis::use_version(version, push=F) }
}
