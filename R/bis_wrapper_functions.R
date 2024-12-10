
#' @title Return BIS Info
#'
#' @description Return a data frame of important BIS calculation.
#' @param .data The data object to calculate BIS from.
#' @param pars The between-subjects parameters to calculate BIS across.
#' @param verbose Should stuff be printed to the console or not?
return_bis_info <- function(.data, pars = list('drift','boundary'),verbose=F) {
  OUT <- NULL
  for (par in pars) {
    x.par = pars[pars!=par][[1]]
    vals = unlist(unique(data[,par]))
    x.vals = unlist(unique(data[,x.par]))
    for (val in vals) {
      val.data = data[data[par]==val,c(x.par,'rt.c','pc')]
      colnames(val.data)[1] = 'target'
      val.data$zPC = (val.data$pc-mean(val.data$pc))/sd(val.data$pc)
      val.data$zRT = (val.data$rt.c-mean(val.data$rt.c))/sd(val.data$rt.c)
      val.data$bis = val.data$zPC - val.data$zRT
      out = data.frame(
            par = par,
            val = val,
          x.val = x.vals,
            bis = aggregate(bis ~ target, val.data, mean)$bis,
         rt.sig = t.test(rt.c ~ target, val.data, TRUE)$p.value <= 0.05,
         pc.sig = t.test(pc ~ target, val.data, TRUE)$p.value <= 0.05,
        bis.sig = t.test(bis ~ target, val.data, TRUE)$p.value <= 0.05
      )
      OUT <- rbind(OUT, out)
      if (verbose) pride(par,val)
    }
  }
  rownames(OUT) = 1:nrow(OUT)
  return(OUT)
}
