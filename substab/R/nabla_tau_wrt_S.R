# Generated from _main.Rmd: do not edit by hand

#' Feature perturbation metric of S1 and S2 w.r.t a collection of selection sets
#' 
#' line (iv) in Algorithm 2
#' 
#' @param X the n by p design matrix
#' @param y the length n response vector
#' @param S1 a vector representing subset S1
#' @param S2 a vector representing subset S2
#' @param Selection_set a list where each element gives a selection set
#' 
#' @return the minimum nabla tau metric of S1 and S2 wrt to all selection sets
#' 
#' @export
nabla_tau_wrt_S <- function(X, y, S1, S2, Selection_set) {
  Nabla_Tau = c()
  for(i in seq_along(Selection_set)) {
    S0 = Selection_set[[i]]
    if(all(S1 %in% S0) | all(S2 %in% S0) ) {
      S0 = setdiff(Selection_set[[i]], union(S1, S2))
      Nabla_Tau = c(Nabla_Tau, nabla_tau(X, y, S1, S2, S0))
    }
  }
  if(length(Nabla_Tau) == 0) stop("Neither S1 nor S2 is in any selection set!")
  return(min(Nabla_Tau))
}
