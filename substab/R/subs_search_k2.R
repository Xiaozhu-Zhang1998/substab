# Generated from _main.Rmd: do not edit by hand

#' Substitute searching algorithm
#' 
#' Algorithm 2. The function only searches for subsets with size (k) at most 2.
#' 
#' @param X the n by p design matrix
#' @param y the length n response vector
#' @param Selection_set a list where each element gives a selection set
#' @param bags a list returned from l0_subsampling
#' @param alpha a number, the threshold for stability
#' @param tau0 a number, the threshold for substitutability (tau) metric
#' @param tau1 a number, the threshold for feature perturbation (nabla tau) metric
#' @param tau2 a number, threshold for degeneracy (tri tau) metric
#' @param maxreturn an integer, the maximum number of requested substitutes
#' @param verbose 0 or 1, logical value for outputting the searching progress
#' 
#' @return a data frame for all substitutes found
#' \describe{
#'  \item{S1}{name of subsets S1}
#'  \item{S2}{name of subsets S2}
#'  \item{tau}{the tau metric of S1 and S2 w.r.t. the selection sets}
#'  \item{nabla_tau}{the nabla_tau metric of S1 and S2 w.r.t. the selection sets}
#'  \item{tri_tau}{the tri tau metric of S1 and S2 w.r.t. the selection sets}
#' }
#' 
#' @export
subs_search_k2 <- function(X, y, Selection_set, bags, alpha, tau0 = 0.8, tau1 = 0.5, tau2 = 0.8, maxreturn = 10, verbose = 1) {
  RS = data.frame(matrix(0, nrow = 0, ncol = 5))
  colnames(RS) = c("S1", "S2", "tau", "nabla_tau", "tri_tau")
  count = 1
  # find all combinations of S1, S2 in selection sets
  combidx_set = combinat::combn(1:length(Selection_set), 2)
  for(i in 1:ncol(combidx_set)) {
    if(verbose == 1) cat("start the", i, "th combination of (S1, S2) out of", ncol(combidx_set), "\n")
    zoom_set = combidx_set[,i]
    set1 = Selection_set[[zoom_set[1]]]
    set2 = Selection_set[[zoom_set[2]]]
    # find all subsets of S1 and S2
    combidx_feat1 = combinat::combn(set1, 2); combidx_feat1 = cbind(combidx_feat1, rep(1, 2) %*% t(set1))
    combidx_feat2 = combinat::combn(set2, 2); combidx_feat2 = cbind(combidx_feat2, rep(1, 2) %*% t(set2))
    for(j in 1:ncol(combidx_feat1)) {
      if(verbose == 1) cat("\t start the", j, "th subset of S1 out of", ncol(combidx_feat1), "\n")
      for(l in 1:ncol(combidx_feat2)) {
        S1 = sort(unique(combidx_feat1[,j]))
        S2 = sort(unique(combidx_feat2[,l]))
        name1 = paste0(S1, collapse = ",")
        name2 = paste0(S2, collapse = ",")
        # check if S1 and S2 are the same, or if they have been selected
        if(all(S1 == S2)) next
        if((name1 %in% RS$S1 & name2 %in% RS$S2) | (name2 %in% RS$S1 & name1 %in% RS$S2)) next
        # check if S1 and S2 are stable together
        joint_supp = stability(X, union(S1, S2), bags)
        if(joint_supp >= alpha) next
        # check if tau < tau0 
        tau_obj = tau_wrt_S(X, y, S1, S2, Selection_set) 
        if(tau_obj < tau0) next
        # check if nabla_tau < tau1
        nabla_tau_obj = nabla_tau_wrt_S(X, y, S1, S2, Selection_set)
        if(nabla_tau_obj < tau1) next
        # check if tri_tau > tau2
        tri_tau_obj = tri_tau_wrt_S(X, y, S1, S2, Selection_set)
        if(tri_tau_obj > tau2) next
        # record
        RS[count,'S1'] = name1
        RS[count,'S2'] = name2
        RS[count,'tau'] = tau_obj
        RS[count,'nabla_tau'] = nabla_tau_obj
        RS[count,'tri_tau'] = tri_tau_obj
        count = count + 1
        if(nrow(RS) >= maxreturn) return(RS)
      }
    }
  }
  return(RS)
}
