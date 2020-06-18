#' identify_chol
#'
#' @param Sigma_res 
#'
#' @return
#' @export
#'
#' @description Computes coeffcient matrix capturing contemporaneous effects through short-run restriction using Cholesky decompostion. 

identify_chol = function (Sigma_res) {
  # Choleski decomposition - lower triangular:
  P = t(chol.default(Sigma_res)) # lower triangular Cholesky factor
  B_0 = solve(P)
  # B_0 %*% Sigma_res %*% t(B_0) # can verify that this is equal to the identity matrix (i.e. structural shocks are orthogonal/uncorrelated)
  return(B_0)
}