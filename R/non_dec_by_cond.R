
#' @title Non-Decision Time by Cond
#'
#' @description Function to calculate non-decision time when it is fixed across conditions.
#' @param prms_model list // One row of prms_matrix or df_prms.
#' @param prms_solve list // The parameters relevant for deriving the PDFs.
#' @param t_vec list // Time space, from 0 to t_max with length nt + 1.
#' @param one_cond single character string // Indicates the current condition.
#' @param ddm_opts Additional arguments to pass to deeper dRiftDM calls.
#' @return A vector of length t_vec containing non_dec from prms_model.
non_dec_by_cond <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts){
  non_dec = prms_model[paste0('non_dec',one_cond)]
  tmax = prms_solve[["t_max"]]
  dt = prms_solve[["dt"]]
  dt = prms_solve[["dt"]]
  d_nt = numeric(length(t_vec))
  which_index = as.integer(non_dec/dt)
  d_nt[which_index + 1] = 1/dt
  return(d_nt)
}

#' @title Fixed Non-Decision Time
#'
#' @description Function to calculate non-decision time when it is fixed across conditions.
#' @param prms_model list // One row of prms_matrix or df_prms.
#' @param prms_solve list // The parameters relevant for deriving the PDFs.
#' @param t_vec list // Time space, from 0 to t_max with length nt + 1.
#' @param one_cond single character string // Indicates the current condition.
#' @param ddm_opts Additional arguments to pass to deeper dRiftDM calls.
#' @return A vector of length t_vec containing non_dec from prms_model.
non_dec_fixed <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts){
  non_dec = prms_model[["non_dec"]]
  tmax = prms_solve[["t_max"]]
  dt = prms_solve[["dt"]]
  dt = prms_solve[["dt"]]
  d_nt = numeric(length(t_vec))
  which_index = as.integer(non_dec/dt)
  d_nt[which_index + 1] = 1/dt
  return(d_nt)
}
