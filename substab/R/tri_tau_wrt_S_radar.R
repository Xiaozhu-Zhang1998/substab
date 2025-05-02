# Generated from _main.Rmd: do not edit by hand

#' Degeneracy metric of S (in the substitutes (S, S_)) and its subset w.r.t a collection of selection sets
#' 
#' @param X the n by p design matrix
#' @param y the length n response vector
#' @param j a vector representing a subset of S
#' @param S a vector representing set S
#' @param S_ a vector representing set S_
#' @param Selection_set a list where each element gives a selection set
#' 
#' @return the degeneracy metric of j and S w.r.t the selection sets
tri_tau_wrt_S_radar <- function(X, y, j, S, S_, Selection_set) {
  Tri_Tau = c()
  for(i in seq_along(Selection_set)) {
    S0 = Selection_set[[i]]
    if(all(S %in% S0) | all(S_ %in% S0) ) {
      S0 = setdiff(Selection_set[[i]], union(S, S_))
      Tri_Tau = c(Tri_Tau, tau(X, y, j, S, S0))
    }
  }
  return(max(Tri_Tau))
}
