# Generated from _main.Rmd: do not edit by hand

#' Generate design matrix with complex dependency
#' 
#' A function that generates a design matrix with complex dependency structures. 
#' The parent features follow N(0,1), and perturbation direction follows N(0, eta^2).
#' 
#' @param n sample size
#' @param nb number of blocks
#' @param b number of parents in each block
#' @param eta standard deviation of the perturbation direction
#' 
#' @return an n by nb*(b+1) matrix
#' 
#' @export
gen_block <- function(n, nb, b, eta = 0.1) {
  X = matrix(stats::rnorm(n*nb*(b+1)), nrow = n)
  id_child = seq(from = b+1, to = nb*(b+1), by = b+1)
  id_parents = setdiff(1:ncol(X), id_child)
  for(i in 1:ncol(X)) {
    if(i %in% id_parents) {
    } else {
      X[,i] = apply(X[,(i-b):(i-1)], 1, sum) + stats::rnorm(n, sd = eta)
    }
  }
  return(X)
}
