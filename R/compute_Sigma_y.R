#' compute_Sigma_y
#'
#' @param Phi
#' @param Sigma_res
#'
#' @return
#' @export
#'
compute_Sigma_y = function(Phi, Sigma_res) {

  n = length(Phi)

  # Pre-allocate ----
  # Let first list element represent Phi_0=I_k
  Sigma_y = lapply(1:n, function(i) {
    diag(0, nrow(Sigma_res), ncol(Sigma_res))
  })
  # Sigma_1:
  Sigma_y[[1]] = Sigma_res

  # Iterate ----
  for (i in 1:(n-1)) {

    Sigma_y[[i+1]] = Sigma_y[[i]] + Phi[[i+1]] %*% Sigma_res %*% t(Phi[[i+1]])

  }

  return(Sigma_y)

}
