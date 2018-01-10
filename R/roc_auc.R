#' @export

roc_auc <- function(TPR, FPR) {
  dTPR <- c(diff(TPR), 0)
  dFPR <- c(diff(FPR), 0)
  sum(TPR * dFPR) + sum(dFPR * dTPR)/2
}
