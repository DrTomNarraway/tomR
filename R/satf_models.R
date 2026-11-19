
#' @title Classic 3-Parameter Exponential SATF Model
#'
#' @description Fit a classic 3-parameter exponential model to data.
#' \deqn{PC(rt) = 50 + asymptote * (1 - exp(-slope * pmax(0, rt - intercept)))}
#' @param .data Data to fit model to. Must have a column for accuracy, labelled "pc" and a colum for response time labelled "rt".
#' @return minpack.lm::nlsLM model fit to .data.
#' @importFrom minpack.lm nlsLM
#' @export
exponential_model <- function(.data) {
  model = minpack.lm::nlsLM(
    pc ~ 50 + asymptote * (1 - exp(-slope * pmax(0, rt - intercept))),
    data = .data,
    start = list(asymptote=95, slope=0.004, intercept=300),
    lower = c(asymptote=0, slope=0.0001, intercept=0),
    upper = c(asymptote=100, slope=0.05, intercept=1000),
    algorithm = "port"
  )
  return(model)
}

#' @title Power-Exponential SATF Model
#'
#' @description Fit a power-exponential model as seen in Reed (1973, 1976).
#' \deqn{PC(rt) = asymptote - (asymptote - 50) * exp(-(rate * pmax(0, rt - delay))^power)}
#' @param .data Data to fit model to. Must have a column for accuracy, labelled "pc" and a colum for response time labelled "rt".
#' @return minpack.lm::nlsLM model fit to .data.
#' @importFrom minpack.lm nlsLM
#' @export
reed_model <- function(.data) {
  model = minpack.lm::nlsLM(
    pc ~ asymptote - (asymptote - 50) * exp(-(rate * pmax(0, rt - delay))^power),
    data = .data,
    start = list(asymptote=95, rate=0.005, delay=300, power=0.3),
    lower = c(asymptote=50, rate=0.0001, delay=0, power=0.1),
    upper = c(asymptote=100, rate=0.1, delay=2000, power=10),
    algorithm = "port"
  )
  return(model)
}

#' @title Sigmoid SATF Model
#'
#' @description Fit a standard sigmoid model to data.
#' \deqn{PC(rt) = 50 + ((asymptote - 50) / (1 + exp(-slope * (rt - midpoint))))}
#' @param .data Data to fit model to. Must have a column for accuracy, labelled "pc" and a colum for response time labelled "rt".
#' @return minpack.lm::nlsLM model fit to .data.
#' @importFrom minpack.lm nlsLM
#' @export
sigmoid_model <- function(.data) {
  model = minpack.lm::nlsLM(
    pc ~ 50 + ((asymptote - 50) / (1 + exp(-slope * (rt - midpoint)))),
    data = .data,
    start = list(asymptote=95, midpoint=400, slope=0.01),
    lower = c(asymptote=50, midpoint=0, slope=0.0001),
    upper = c(asymptote=100, midpoint=2000, slope=0.1),
    algorithm = "port"
  )
  return(model)
}

#' @title Weibull SATF Model
#'
#' @description Fit a standard weibull model to data.
#' \deqn{PC(rt) = asymptote - (asymptote - 50) * exp(-(rt / scale)^shape)}
#' @param .data Data to fit model to. Must have a column for accuracy, labelled "pc" and a colum for response time labelled "rt".
#' @return minpack.lm::nlsLM model fit to .data.
#' @importFrom stats nls
#' @export
weibull_model <- function(.data) {
  model = stats::nls(
    pc ~ asymptote - (asymptote - 50) * exp(-(rt / scale)^shape),
    data = .data,
    start = list(asymptote=100, scale=300, shape=2),
    lower = c(asymptote=50, scale=0, shape=0),
    upper = c(asymptote=100, scale=2000, shape=10),
    algorithm = "port"
  )
  return(model)
}

#' @title Linear SATF Model
#'
#' @description Fit a linear model to data.
#' @param .data Data to fit model to. Must have a column for accuracy, labelled "pc" and a colum for response time labelled "rt".
#' @return stats::lm model fit to .data.
#' @importFrom stats lm
#' @export
linear_model <- function(.data) {
  model = stats::lm(pc ~ rt, data=.data)
  return(model)
}
