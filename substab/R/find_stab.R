# Generated from _main.Rmd: do not edit by hand

#' The subspace stability of a column space
#' 
#' @param base_lst a list with length 2*B, where each element gives the n by n projection matrix \eqn{P_{X_S^l}} from the l-th subsample
#' @param Z a matrix with linearly independent columns
#' 
#' @return the subspace stability value of the column space of Z
find_stab <- function(base_lst, Z) {
  Uc = svd(Z)$u
  Mat = matrix(0, nrow = nrow(Z), ncol = nrow(Z))
  for(i in seq_along(base_lst)) {
    Ub = base_lst[[i]]
    prod = t(Uc) %*% Ub
    Mat = Mat + Uc %*% (prod %*% t(prod)) %*% t(Uc)
  }
  Mat = Mat / length(base_lst)
  min( RSpectra::svds(Mat, ncol(Z), 0, 0)$d )
}
