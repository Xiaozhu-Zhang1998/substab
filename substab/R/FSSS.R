# Generated from _main.Rmd: do not edit by hand

#' FSSS algorithm
#' 
#' Algorithm 1. A function that implements the FSSS algorithm
#' 
#' @param X the n by p design matrix
#' @param bags a list returned from l0_subsampling
#' @param alpha a value in (0,1), the threshold for stability with default value 0.9
#' @param type a character, either "greedy" (greedy algorithm) or "any-path" 
#' @param verbose 0 or 1, logical value for outputting the selection path
#' 
#' @return a vector representing the selection set
#' 
#' @export
FSSS <- function(X, bags, alpha = 0.9, type = "greedy", verbose = 1) {
  Pavg = bags$Pavg
  base_lst = bags$base_lst
  d = ncol(X)
  
  if(nrow(Pavg) != nrow(X) ) stop("The Pavg has different dimension with X!")
  if(!(type %in% c("greedy", "any-path"))) stop("The type of FSSS must be either 'greedy' or 'any-path'!")
  
  node_par = c()
  orth_par = NULL
  stability_output = 0
  
  while(TRUE) {
    if(length(node_par) == d) {
      break
    }
    node_chil = lapply(setdiff(1:d, node_par), function(i) {
      c(node_par, i)
    })
    if (is.null(orth_par)) {
      dir_chil = lapply(node_chil, function(x) {
        X[,x]
      })
    } else {
      dir_chil = lapply(node_chil, function(x) { # find the gs-orthogonal vector
        Xj = X[, setdiff(unlist(x), node_par)]
        terms = lapply(seq_len(ncol(orth_par)), function(i) {
          sum(Xj * orth_par[,i]) / sum(orth_par[,i]^2) *  matrix(orth_par[,i])
        })
        rs = Xj - apply( matrix( unlist(terms), ncol = ncol(orth_par) ), 1, sum)
        return(rs)
      })
    }
    psi_chil = sapply(dir_chil, function(x) {
      if(norm(unlist(x), "2") < 1e-7) {
        return(1)
      } else {
        return( 1 - t(unlist(x)) %*% Pavg %*% unlist(x) / norm(unlist(x), "2")^2 )
      }
    })
    
    if(type == "greedy") {
      # for the greedy path
      idx = which.min(psi_chil)
      stability = find_stab(base_lst, X[,node_chil[[idx]], drop = FALSE])
    } else {
      # for any path
      candidate = which(psi_chil < 1 - alpha)
      while(length(candidate) > 0) {
        idx = ifelse(length(candidate) != 1, sample(candidate, 1), candidate)
        stability = find_stab(base_lst, X[,node_chil[[idx]], drop = FALSE])
        if( stability >= alpha & psi_chil[idx] <= 1 - alpha ) {
          break
        } else {
          candidate = setdiff(candidate, idx)
        }
      }
      
      if(length(candidate) == 0) break
    }
    
    # add feature to selection set
    if( psi_chil[idx] <= 1 - alpha & stability >= alpha) {
      orth_par = cbind( orth_par, dir_chil[[idx]])
      node_par = node_chil[[idx]]
      if(verbose == 1) cat("current selection set:", node_par, "\n")
    } else {
      break
    }
  }
  
  return(node_par)
}
