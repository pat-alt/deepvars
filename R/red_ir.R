#' red_ir
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
#' @description Computes the reduced-form impulse responses. These are denoted as by Phi in Kilian, Luetkepohl.
#'
red_ir = function(lags, K, A_comp, const, n.ahead) {

  J = compute_J(K, lags)

  Phi = lapply(1:n.ahead, function(i) {
    J %*% (A_comp %^% (i-1)) %*% t(J)
  })

  return(Phi)

}
