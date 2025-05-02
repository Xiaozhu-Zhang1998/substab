# Generated from _main.Rmd: do not edit by hand

#' Degeneracy metric of S1 and S2 w.r.t a collection of selection sets
#' 
#' line (v) in Algorithm 2
#' 
#' @param X the n by p design matrix
#' @param y the length n response vector
#' @param S1 a vector representing subset S1
#' @param S2 a vector representing subset S2
#' @param Selection_set a list where each element gives a selection set
#' 
#' @return the minimum tri tau metric of S1 and S2 wrt to all selection sets
#' 
#' @export
tri_tau_wrt_S <- function(X, y, S1, S2, Selection_set) {
  Tri_Tau = c()
  for(i in seq_along(Selection_set)) {
    S0 = Selection_set[[i]]
    if(all(S1 %in% S0) | all(S2 %in% S0) ) {
      S0 = setdiff(Selection_set[[i]], union(S1, S2))
      Tri_Tau = c(Tri_Tau, tri_tau(X, y, S1, S2, S0))
    }
  }
  if(length(Tri_Tau) == 0) stop("Neither S1 nor S2 is in any selection set!")
  max(Tri_Tau)
}
