
#' @title Correlation from Standard Deviations
#'
#' @description Calculate the correlation of parameters given a known between- and within-subjects standard deviation.
#' @param subject.sd numeric // The known between-subjects standard deviation.
#' @param cond.sd numeric // The known within-subjects standard deviation.
#' @returns numeric // The correlation of parameters.
r_from_sds <- function(subject.sd, cond.sd) {return(subject.sd^2 / (subject.sd^2 + cond.sd^2))}

#' @title SD(Cond) from Correlation
#'
#' @description Calculate the condition-level standard deviation required to reach the desired correlation.
#' @param sd numeric // The known standard deviation, either between- or within-subjects.
#' @param r numeric // The desired correlation.
#' @returns numeric // The unknown standard deviation.
sd.cond_from_r <- function(sd, r) {return((sd * sqrt(r*(1-r))) / r)}

#' @title SD(Subject) from Correlation
#'
#' @description Calculate the subject-level standard deviation required to reach the desired correlation.
#' @param sd numeric // The known standard deviation, either between- or within-subjects.
#' @param r numeric // The desired correlation.
#' @returns numeric // The unknown standard deviation.
sd.subject_from_r <- function(sd, r) {return(return((sd * sqrt(r*(1-r))) / (1-r)))}
