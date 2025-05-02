# Generated from _main.Rmd: do not edit by hand

#' Degeneracy metric of S1 and S2 w.r.t S0
#' 
#' Definition 5
#' 
#' @param X the n by p design matrix
#' @param y the length n response vector
#' @param S1 a vector representing subset S1
#' @param S2 a vector representing subset S2
#' @param S0 a vector representing subset S0
#' 
#' @return the degeneracy (tri tau) metric
#' 
#' @export
tri_tau <- function(X, y, S1, S2, S0) {
  if(length(S1) == 1 & length(S2) == 1) return(0)
  PS1 = rje::powerSet(S1)
  PS2 = rje::powerSet(S2)
  Tau.pair1 = c()
  Name.pair1 = list()
  for(j in PS1[2:(length(PS1)-1)]) {
    rs = tau(X, y, j, S1, S0)
    Tau.pair1 = c(Tau.pair1, rs)
  }
  Tau.pair2 = c()
  Name.pair2 = list()
  for(j in PS2[2:(length(PS2)-1)]) {
    rs = tau(X, y, j, S2, S0)
    Tau.pair2 = c(Tau.pair2, rs)
  }
  return(max(Tau.pair1, Tau.pair2))
}
