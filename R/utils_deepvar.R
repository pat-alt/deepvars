#' compute_Phi
#'
#' @param K
#' @param A_comp
#' @param const
#' @param n.ahead
#'
#' @importFrom expm `%^%`
#'
#' @return
#'
#' @description Computes the Phi coefficients (reduced form) in Kilian, Luetkepohl.
#'
compute_Phi = function(deepvar_model, n.ahead) {

  J = compute_J(K, lags)

  Phi = lapply(1:n.ahead, function(i) {
    J %*% (A_comp %^% (i-1)) %*% t(J)
  })

  return(Phi)

}
