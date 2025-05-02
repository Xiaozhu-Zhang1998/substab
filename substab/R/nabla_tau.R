# Generated from _main.Rmd: do not edit by hand

#' Feature perturbation metric of S1 and S2 w.r.t S0
#' 
#' Definition 4
#' 
#' @param X the n by p design matrix
#' @param y the length n response vector
#' @param S1 a vector representing subset S1
#' @param S2 a vector representing subset S2
#' @param S0 a vector representing subset S0
#' 
#' @return the feature perturbation (nabla tau) metric
#' 
#' @export
nabla_tau <- function(X, y, S1, S2, S0) {
  if(length(S1) == 1 & length(S2) == 1) return(1)
  tau.mat = matrix(0, nrow = length(S1), ncol = length(S2))
  for(i in 1:nrow(tau.mat)) {
    for(j in 1:ncol(tau.mat)) {
      tau.mat[i,j] = tau(X, y, S1[i], S2[j], S0)
    }
  }
  tau.val = c()
  for(i in 1:min(length(S1), length(S2))) {
    tau.val = c(tau.val, max(tau.mat))
    idx = which(tau.mat == max(tau.mat), arr.ind = TRUE)[1,]
    tau.mat = tau.mat[-idx[1], ,drop = FALSE]
    tau.mat = tau.mat[,-idx[2], drop = FALSE]
  }
  tau_overall = tau(X, y, S1, S2, S0)
  if( abs(max(min(tau.val), tau_overall)) < 1e-7 ) return(0)
  return( abs(min(tau.val) - tau_overall) / max(min(tau.val), tau_overall) )
}
