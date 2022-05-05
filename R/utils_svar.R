# SVAR utils (deprecated) ----
# NOTE: These functions are deprecated and therefore not exported.

#' identify_chol
#'
#' @param Sigma_res
#'
#' @return
#'
#' @description Computes coefficient matrix capturing contemporaneous effects through short-run restriction using Cholesky decomposition.

identify_chol = function (Sigma_res) {
  # Choleski decomposition - lower triangular:
  P = t(chol.default(Sigma_res)) # lower triangular Cholesky factor
  B_0 = solve(P)
  # B_0 %*% Sigma_res %*% t(B_0) # can verify that this is equal to the identity matrix (i.e. structural shocks are orthogonal/uncorrelated)
  return(B_0)
}

#' compute_Theta
#'
#' @param Phi
#' @param B_0
#'
#' @return
#'
#' @description Helper function used to compute Theta coefficients (structural) as in Kilian, Luetkepohl
compute_Theta = function(Phi, B_0) {

  Theta = lapply(1:length(Phi), function(i) {
    Phi[[i]] %*% solve(B_0)
  })

  return(Theta)

}

#' compute_theta_kj_sq
#'
#' @param Theta
#' @param n.ahead
#'
#' @return
#'
#' @details Helper function used to compute orthogonalised IRFs as in Kilian, Luetkepohl.
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

#' compute_mspe
#'
#' @param coeffs
#' @param n.ahead
#'
#' @return
compute_mspe = function(coeffs, n.ahead=10)

{
  mspe = lapply(1:n.ahead, function(h) {
    terms_to_sum = lapply(1:h, function(i) {
      tcrossprod(coeffs[[i]])
    })
    mspe_h = Reduce(`+`, terms_to_sum)
  })
  return(mspe)
}

#' compute_Sigma_y
#'
#' @param Phi
#' @param Sigma_res
#'
#' @return
compute_Sigma_y = function(Phi, Sigma_res) {

  n = length(Phi)

  # Pre-allocate
  # Let first list element represent Phi_0=I_k
  Sigma_y = lapply(1:n, function(i) {
    diag(0, nrow(Sigma_res), ncol(Sigma_res))
  })
  # Sigma_1:
  Sigma_y[[1]] = Sigma_res

  # Iterate
  for (i in 1:(n-1)) {

    Sigma_y[[i+1]] = Sigma_y[[i]] + Phi[[i+1]] %*% Sigma_res %*% t(Phi[[i+1]])

  }

  return(Sigma_y)

}
