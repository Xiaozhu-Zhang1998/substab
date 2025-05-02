# Generated from _main.Rmd: do not edit by hand

#' Substitutability metric of S1 and S2 w.r.t S0
#' 
#' Definition 3
#' 
#' @param X the n by p design matrix
#' @param y the length n response vector
#' @param S1 a vector representing subset S1
#' @param S2 a vector representing subset S2
#' @param S0 a vector representing subset S0
#' 
#' @return the substitutability (tau) metric
#' 
#' @export
tau <- function(X, y, S1, S2, S0) {
  U = svd(X[, S0])$u
  U1 = svd(X[, union(S0, S1)])$u
  U2 = svd(X[, union(S0, S2)])$u
  u1 = U1 %*% (t(U1) %*% y) - U %*% (t(U) %*% y)
  u2 = U2 %*% (t(U2) %*% y) - U %*% (t(U) %*% y)
  u.vec = list(u1, u2)
  if(norm(u1, "2") < norm(u2, "2")) {
    u1 = u.vec[[2]]
    u2 = u.vec[[1]]
  }
  if( abs(norm(u1, "2")) < 1e-7 ) return(0)
  return(as.numeric(t(u1) %*% u2 / norm(u1, "2")^2 ) )
}
