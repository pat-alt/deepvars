#' compute_J
#'
#' @param K
#' @param lag
#'
#' @return
#' @export
#'
#' @details Helper function to create the allocation matrix J, which can be used to transform a $VAR(p)$ into $VMA$ represntation.
#'
compute_J = function(K, lag) {

  J = matrix(0,nrow=K,ncol=K*lags)
  diag(J) = 1
  return(J)

}
