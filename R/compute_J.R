#' compute_J
#'
#' @param K
#' @param lag
#'
#' @return
#' @export
#'
compute_J = function(K, lag) {

  J <- matrix(0,nrow=K,ncol=K*lags)
  diag(J) <- 1
  return(J)

}
