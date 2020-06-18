#' compute_Phi
#'
#' @param lag
#' @param K
#' @param A_comp
#' @param const
#' @param n.ahead
#'
#' @importFrom expm `%^%`
#'
#' @return
#' @export
#'
#' @description Helper function used to compute Phi coefficients as in Kilian, Luetkepohl
#'
compute_Phi = function(lag, K, A_comp, const, n.ahead) {

  J = compute_J(K, lag)

  Phi = lapply(1:n.ahead, function(i) {
    J %*% (A_comp %^% (i-1)) %*% t(J)
  })

  return(Phi)

}
