
#' @title Drift Rate By Condition
#'
#' @description Function to calculate muc when it differs by condition.
#' @param prms_model list // One row of prms_matrix or df_prms.
#' @param prms_solve list // The parameters relevant for deriving the PDFs.
#' @param t_vec list // Time space, from 0 to t_max with length nt + 1.
#' @param one_cond single character string // Indicates the current condition.
#' @param ddm_opts Additional arguments to pass to deeper dRiftDM calls.
#' @return A vector of length t_vec containing muc from prms_model based on one_cond.
muc_by_cond <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  return(rep(prms_model[paste0('muc',one_cond)], length(t_vec)))
}

#' @title Fixed Drift Rate
#'
#' @description Function to calculate muc when it is fixed across conditions.
#' @param prms_model list // One row of prms_matrix or df_prms.
#' @param prms_solve list // The parameters relevant for deriving the PDFs.
#' @param t_vec list // Time space, from 0 to t_max with length nt + 1.
#' @param one_cond single character string // Indicates the current condition.
#' @param ddm_opts Additional arguments to pass to deeper dRiftDM calls.
#' @return A vector of length t_vec containing muc from prms_model.
muc_fixed <- function(prms_model, prms_solve, t_vec, one_cond, ddm_opts) {
  return(rep(prms_model[["muc"]], length(t_vec)))
}
