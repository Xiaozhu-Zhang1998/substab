# Generated from _main.Rmd: do not edit by hand

#' The subspace stability of a selection set
#' 
#' Definition 2
#' 
#' @param X the n by p design matrix
#' @param S a vector representing selection set
#' @param bags a list returned from l0_subsampling
#' 
#' @return the subspace stability value of S
#' 
#' @export
stability <- function(X, S, bags) {
  base_lst = bags$base_lst
  if(nrow(bags$Pavg) != nrow(X) ) stop("The Pavg has different dimension with X!")
  find_stab(base_lst, X[,S, drop = FALSE])
}
