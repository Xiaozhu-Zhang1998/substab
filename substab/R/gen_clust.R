# Generated from _main.Rmd: do not edit by hand

#' Generate cluster design matrix
#' 
#' A function that generates a design matrix with cluster structures. 
#' The representative feature follows N(0,1), and perturbation direction follows
#'  N(0, eta^2).
#' 
#' @param n sample size
#' @param s number of clusters
#' @param d number of features in each cluster
#' @param eta standard deviation of the perturbation direction
#' 
#' @return an n by s*d matrix
#' 
#' @export
gen_clust <- function(n, s, d, eta = 0.1) {
  X = matrix(stats::rnorm(n*s*d), nrow = n)
  
  id_rep = (1:s) * d - (d-1)
  id_prox = setdiff(1:ncol(X), id_rep)
  track_rep = 0
  for(i in 1:ncol(X)) {
    if(i %in% id_rep) {
      track_rep = i
    } else {
      X[,i] = X[,track_rep] + stats::rnorm(n, sd = eta)
    }
  }
  return(X)
}
