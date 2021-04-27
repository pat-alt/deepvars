#' compute_theta_kj_sq
#'
#' @param Theta
#' @param n.ahead
#'
#' @return
#' @export
#'
#' @details Helper function used to compute orthogonalised IRFs as in Kilian, Luetkepohl. These represent the reduced form impulse response functions.
compute_theta_kj_sq = function(Theta, n.ahead) {

  theta_kj_sq = lapply(1:n.ahead, function(h) { # loop over h time periods
    out = sapply(1:ncol(Theta[[h]]), function(k) {
      terms_to_sum = lapply(1:h, function(i) {
        Theta[[i]][k,]**2
      })
      theta_kj_sq_h = Reduce(`+`, terms_to_sum)
    })
    colnames(out) = colnames(Theta[[h]])
    return(out)
  })

  return(theta_kj_sq)
}
