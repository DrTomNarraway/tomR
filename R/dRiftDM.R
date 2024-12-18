
#' @title Quick Subject Simulation
#'
#' @description Perform a very quick simulation of one subject using dRiftDM.
#' @param n The number of trials to simulate.
#' @returns Returns a data frame with columns 'rt' and 'correct'.
quick.subject.sim <- function(n=100) {
  model <- dRiftDM::ratcliff_dm()
  out <- dRiftDM::simulate_data(model, n)
  out$Score[out$Error==0] <- 1
  out$Score[out$Error==1] <- 0
  out <- out[, c('RT','Score')]
  return(out)
}

#' @title Quick Group Simulation
#'
#' @description Perform a very quick simulation of one subject using dRiftDM.
#' @param n The number of trials to simulate per subject
#' @param N the number of subjects to simulate.
#' @returns Returns a data frame with columns 'ID','RT', and 'Score'.
quick.group.sim <- function(n=100, N=20) {
  model <- dRiftDM::ratcliff_dm()
  model <- dRiftDM::set_solver_settings(model, c(dx=0.005, dt=0.005))
  df_prms <- data.frame(
    muc = rep(model$prms_model['muc'], N),
    b = rep(model$prms_model['b'], N),
    non_dec = rep(model$prms_model['non_dec'], N),
    ID = 1:N
  )
  out <- dRiftDM::simulate_data(model, n, df_prms=df_prms)
  out$Score[out$Error==0] <- 1
  out$Score[out$Error==1] <- 0
  out <- out[, c('ID','RT','Score')]
  return(out)
}

#' @title Drift Rate By Condition
#'
#' @description Function to calculate muc when it differs by condition.
muc_by_cond <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  return(rep(prms_model[paste0('muc_',one_cond)], length(t_vec)))
}

#' @title Boundary By Condition
#'
#' @description Function to calculate b when it differs by condition.
b_by_cond <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  return(rep(prms_model[paste0('b_',one_cond)], length(t_vec)))
}
