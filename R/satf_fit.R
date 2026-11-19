
#' @title SATF Model Fit (R^2)
#'
#' @description Fit parameter calculated based on Dosher (1982) at doi: https://psycnet.apa.org/doi/10.1037/0278-7393.8.3.173.
#' \deqn{R^2 = 1 - sum((pc - ppc)^2 / (N - K)) / sum((pc - mpc)^2 / (N - 1))}
#' @param model SATF model to fit data to.
#' @param .data Data to compare model fit to. Must have a column for accuracy, labelled "pc".
#' @param signifs Numeric. Number of significant figures to round output to.
#' @return R^2, rounded to signif significant figures.
#' @importFrom stats coef predict
#' @export
R2 <- function(model, .data, signifs=3) {
  pc = .data$pc
  ppc = stats::predict(model, .data)
  mpc = mean(ppc)
  N = length(ppc)
  K = length(stats::coef(model))
  R2 = 1 - sum((pc - ppc)^2 / (N - K)) / sum((pc - mpc)^2 / (N - 1))
  return(signif(R2, signifs))
}
