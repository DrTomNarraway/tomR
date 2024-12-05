
#' @title Return BIS Vector.
#'
#' @description BIS wrapper for 2024 within-subjects simulation project.
#' @param .data data.frame to act on.
#' @returns Returns a vector of length nrow(data).
return_bis_vector <- function(.data) {
  .pars=c('drift','boundry')
  out = rep(NA,nrow(.data))
  # for each parameter par
  for (i in 1:length(.pars)) {
    par            = .pars[i]
    other.par      = .pars[.pars!=par]
    vals           = c(unique(.data[,par]))[[1]]
    other.par.vals = c(unique(.data[,other.par]))[[1]]
    # for each value val of the parameter par
    for (j in 1:length(vals)) {
      val = vals[j]
      val.data = .data[.data[,par]==val,]
      # percentage correct
      pc.mean = mean(val.data$pc)
      pc.sd   = sd(val.data$pc)
      z.pc    = (val.data$pc-pc.mean)/pc.sd
      # response time
      rt.mean = mean(val.data$rt.c)
      rt.sd   = sd(val.data$rt.c)
      z.rt    = (val.data$rt.c-rt.mean)/rt.sd
      # bis
      bis = z.pc-z.rt
      # append output
      out[.data[par]==val] = mean(bis)
    } # end of this value
  } # end of this parameter
  return(out)
}

#' @title Return BIS Matrix
#'
#' @description BIS wrapper for 2024 within-subjects simulation project.
#' @param .data data.frame to act on.
#' @returns Returns a matrix of BIS values across possible parameter combinations.
return_bis_matrix <- function(.data) {
  pars = c('drift','boundry')
  # gather parameter values
  drift.vals   = c(unique(.data[,'drift']))[[1]]
  boundry.vals = c(unique(.data[,'boundry']))[[1]]
  # set up the matrices
  drift.matrix   = matrix(NA,ncol=length(drift.vals),nrow=length(boundry.vals))
  colnames(drift.matrix) = drift.vals
  rownames(drift.matrix) = boundry.vals
  boundry.matrix = matrix(NA,ncol=length(boundry.vals),nrow=length(drift.vals))
  colnames(boundry.matrix) = boundry.vals
  rownames(boundry.matrix) = drift.vals
  # for each parameter par
  for (i in 1:length(pars)) {
    par        = pars[i]
    other.par  = pars[pars!=par]
    vals       = c(unique(.data[,par]))[[1]]
    other.vals = c(unique(.data[,other.par]))[[1]]
    # for each value val of the parameter par
    for (j in 1:length(vals)) {
      val = vals[j]
      # set up data object
      val.data = .data[.data[,par]==val,c(other.par,'rt.c','pc')]
      colnames(val.data)[colnames(val.data)==other.par] = 'target'
      # percentage correct
      pc.mean = mean(val.data$pc)
      pc.sd   = sd(val.data$pc)
      z.pc    = (val.data$pc-pc.mean)/pc.sd
      # response time
      rt.mean = mean(val.data$rt.c)
      rt.sd   = sd(val.data$rt.c)
      z.rt    = (val.data$rt.c-rt.mean)/rt.sd
      # bis
      val.data$bis = z.pc-z.rt
      bbo          = aggregate(bis~target,data=val.data,FUN=mean)$bis
      # append to matrix
      if (par=='drift')   drift.matrix[j,]   = bbo
      if (par=='boundry') boundry.matrix[,j] = bbo
    } # end of this value
  } # end of this parameter
  out = list(drift=drift.matrix,boundry=boundry.matrix)
  return(out)
}

#' @title Return BIS List.
#'
#' @description BIS wrapper for 2024 within-subjects simulation project.
#' @param .data data.frame to act on.
#' @returns Returns a list of parameters, with the absolute value of the bis score across the other parameter levels.
return_bis_list <- function(.data) {
  pars = c('drift','boundry')
  # gather parameter values
  drift.vals   = c(unique(.data[,'drift']))[[1]]
  boundry.vals = c(unique(.data[,'boundry']))[[1]]
  # set up the lists
  drift.list          = rep(NA, length(boundry.vals))
  names(drift.list)   = boundry.vals
  boundry.list        = rep(NA, length(drift.vals))
  names(boundry.list) = drift.vals
  # for each parameter par
  for (i in 1:length(pars)) {
    par        = pars[i]
    other.par  = pars[pars!=par]
    vals       = c(unique(.data[,par]))[[1]]
    other.vals = c(unique(.data[,other.par]))[[1]]
    # for each value val of the parameter par
    for (j in 1:length(vals)) {
      val = vals[j]
      # set up data object
      val.data = .data[.data[,par]==val,c(other.par,'rt.c','pc')]
      colnames(val.data)[colnames(val.data)==other.par] = 'target'
      # percentage correct
      pc.mean = mean(val.data$pc)
      pc.sd   = sd(val.data$pc)
      z.pc    = (val.data$pc-pc.mean)/pc.sd
      # response time
      rt.mean = mean(val.data$rt.c)
      rt.sd   = sd(val.data$rt.c)
      z.rt    = (val.data$rt.c-rt.mean)/rt.sd
      # bis
      val.data$bis = z.pc-z.rt
      bbo          = aggregate(bis~target,data=val.data,FUN=mean)$bis
      bbo.max      = max(bbo)
      # append to matrix
      if (par=='drift')   drift.list[j]   = bbo.max
      if (par=='boundry') boundry.list[j] = bbo.max
    } # end of this value
  } # end of this parameter
  out = list(drift=drift.list,boundry=boundry.list)
  return(out)
}

#' @title Return T-Tests.
#'
#' @description T-Test wrapper for 2024 within-subjects simulation project.
#' @param .data data.frame to act on.
#' @returns Returns a matrix of bools showing if the outcome of the t-test was significant or not.
return_tts <- function(.data) {
  if (!('bis') %in% colnames(.data)) return('bis col not found, please append first.')
  pars = c('drift','boundry')
  # set up drift rate matrix
  drift.vals = c(unique(.data[,'drift']))[[1]]
  drift.sigs = matrix(NA, ncol=3, nrow=length(drift.vals))
  rownames(drift.sigs) = drift.vals
  colnames(drift.sigs) = c("rt","pc","bis")
  # set up boundry rate matrix
  boundry.vals = c(unique(.data[,'boundry']))[[1]]
  boundry.sigs = matrix(NA, ncol=3, nrow=length(boundry.vals))
  rownames(boundry.sigs) = boundry.vals
  colnames(boundry.sigs) = c("rt","pc","bis")
  # for each parameter par
  for (i in 1:length(pars)) {
    par = pars[i]
    other.par = pars[pars!=par]
    vals = c(unique(.data[,par]))[[1]]
    # for each value val of the parameter par
    for (j in 1:length(vals)) {
      val = vals[j]
      val.data = .data[.data[,par]==val,c(other.par,'rt.c','pc','bis')]
      colnames(val.data)[colnames(val.data)==other.par] = 'target'
      # perform t tests
      rt.sig  = stats::t.test(rt.c~target,data=val.data)$p.value<=0.05
      pc.sig  = stats::t.test(pc~target,data=val.data)$p.value<=0.05
      bis.sig = stats::t.test(bis~target,data=val.data)$p.value<=0.05
      # store output as appropriate
      if (par=='drift') {
        drift.sigs[j,'rt']  = rt.sig
        drift.sigs[j,'pc']  = pc.sig
        drift.sigs[j,'bis'] = bis.sig
      }
      if (par=='boundry') {
        boundry.sigs[j,'rt']  = rt.sig
        boundry.sigs[j,'pc']  = pc.sig
        boundry.sigs[j,'bis'] = bis.sig
      }
    } # end of this value
  } # end of this parameter
  out <- list(
    drift.sigs    = drift.sigs,
    boundry.sigs = boundry.sigs
  )
  return(out)
}
