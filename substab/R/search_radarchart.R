# Generated from _main.Rmd: do not edit by hand

#' Radar chart of substitute (S1, S2)
#' 
#' Both S1 and S2 need to have size at most 2. 
#' The radar chat shows the most degenerate `topk` subsets of S1 and S2 respectively
#' 
#' @param S1 a vector representing set S1
#' @param S2 a vector representing set S2
#' @param X the n by p design matrix
#' @param y the length n response vector
#' @param Selection_set a list where each element gives a selection set
#' @param topk an integer specifying the maximal number of subsets shown on the plot
#' 
#' @export
search_radarchart <- function(S1, S2, X, y, Selection_set, topk = 2) {
  Tau = matrix(0, nrow = 3, ncol = 5)
  Tau[1,] = matrix(1, 1, 5) 
  Tau[2,] = matrix(0, 1, 5)
  if(min(length(S1), length(S2)) == 1 ) stop("Both S1 and S2 are of length 1")
  PS1 = rje::powerSet(S1)
  PS2 = rje::powerSet(S2)
  Tau.pair1 = c()
  Name.pair1 = list()
  for(j in PS1[2:(length(PS1)-1)]) {
    rs = tri_tau_wrt_S_radar(X, y, j, S1, S2, Selection_set)
    Tau.pair1 = c(Tau.pair1, rs)
    Name.pair1 = c(Name.pair1, list(j))
  }
  Tau.pair2 = c()
  Name.pair2 = list()
  for(j in PS2[2:(length(PS2)-1)]) {
    rs = tri_tau_wrt_S_radar(X, y, j, S2, S1, Selection_set)
    Tau.pair2 = c(Tau.pair2, rs)
    Name.pair2 = c(Name.pair2, list(j))
  }
  if (requireNamespace("tensr", quietly = TRUE)) {
    # the function tensr:::topK is unexported, so we use getFromNamespace
    # however, doing so makes devtools::check() complain about tensr being
    # imported but not used.  We therefore add tensr to "Suggests" instead of
    # Imports.  For that reason we have to check that tensr is actually installed.
    topK <- utils::getFromNamespace("topK", "tensr")
    idx1 = topK(Tau.pair1, min(topk, length(Tau.pair1)))
    idx2 = topK(Tau.pair2, min(topk, length(Tau.pair2)))
  } else {
    stop("Package 'tensr' is required but not installed.")
  }
  Tau[3,] = c(Tau.pair1[idx1], Tau.pair2[idx2], max(Tau.pair1, Tau.pair2))
  Tau = data.frame(Tau)
  name1 = paste0(S1, collapse = ",")
  name2 = paste0(S1, collapse = ",")
  rownames(Tau) = c("1", "2", paste0("S1={", name1, ", S2=", name2, "}" ))
  colnames(Tau) = c(
    sapply(1:length(idx1), function(k) {
      paste0("[{", paste0(Name.pair1[idx1][[k]], collapse = ","), "}, S1]") 
    }),
    sapply(1:length(idx2), function(k) {
      paste0("[{", paste0(Name.pair2[idx2][[k]], collapse = ","), "}, S2]") 
    }),
    "Triangle"
  )
  fmsb::radarchart(
    Tau,
    pfcol = c("#99999980",NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    # title = row.names(Tau)[3],
    axislabcol = "blue", 
    caxislabels = c(0.2, 0.4, 0.6, 0.8, 1),
    vlabels = colnames(Tau),
    vlcex = 1, calcex = 0.9,
    axistype = 1,
  )
  text = paste0("S1 = {", paste0(S1, collapse = ","), "}, S2 = {", paste0(S2, collapse = ","), "}")
  graphics::mtext(text, side=1)
}
