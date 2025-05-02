# Generated from _main.Rmd: do not edit by hand

#' Subsampling using l0 base procedures
#' 
#' @param X the n by p design matrix
#' @param y the length n response vector
#' @param s0 an integer, tuning parameter for l0: the number of selected features
#' @param num_bags number of complementary subsample pairs B
#' 
#' @return a list:
#' \describe{
#' \item{Selset}{a \eqn{2*B} by p matrix, where each row gives the estimated coefficients of each subsample}
#' \item{Pavg}{an n by n matrix, representing the Pavg matrix}
#' \item{base_lst}{a list with length \eqn{2*B}, where each element gives the n by n projection matrix \eqn{P_{X_S^l}} from the l-th subsample}
#' }
#' @export
l0_subsampling <- function(X, y, s0, num_bags = 100){
  n = nrow(X)
  p = ncol(X)
  Pavg <- matrix(0, n, n)
  Selset = matrix(0, 2*num_bags, p)
  base_lst = list()
  
  count = 1
  for (bag_iter in 1:num_bags){
    
    # complementary samples
    ind <- sample(1:n, n, replace = FALSE)
    ind_1 <- ind[1:(n/2)]
    ind_2 <- ind[as.numeric(n/2+1):n]
    
    # l0 model on first dataset: updating frequency of variables and subspaces
    l0_reg_obj = l0_reg(X = X, y = y, ind = ind_1, s0 = s0, intercept = FALSE)
    if(!is.null(l0_reg_obj$U)) {
      Selset[count,] = l0_reg_obj$coef
      base_lst = c(base_lst, list(l0_reg_obj$U))
      Pavg <- Pavg + l0_reg_obj$U %*% t(l0_reg_obj$U)
    }
    count = count + 1
    
    # l0 model on second dataset: updating frequency of variables and subspaces
    l0_reg_obj = l0_reg(X = X, y = y, ind = ind_2, s0 = s0, intercept = FALSE)
    if(!is.null(l0_reg_obj$U)) {
      Selset[count,] = l0_reg_obj$coef
      base_lst = c(base_lst, list(l0_reg_obj$U))
      Pavg <- Pavg + l0_reg_obj$U %*% t(l0_reg_obj$U)
    }
    count = count + 1
    
  }
  Pavg <- Pavg/(2*num_bags)
  
  return(list(Selset = Selset,
              Pavg = Pavg,
              base_lst = base_lst))
}
