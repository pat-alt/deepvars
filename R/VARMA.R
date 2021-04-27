#' VARMA
#'
#' @param varresult
#'
#' @return
#' @export
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @description Helper function used to compute components of VARMA representation
#'
VARMA = function(varresult) {

  # Get outputs ----
  data = data.table::as.data.table(varresult$data)
  constant = varresult$constant # boolean
  const = ifelse(constant, 1, 0)
  lag = varresult$lag
  A = varresult$A
  A_comp = varresult$A_comp
  K = varresult$K
  df = varresult$df
  Sigma_res = varresult$Sigma_res
  var_names = varresult$var_names
  N = varresult$N
  u = varresult$res
  Y = varresult$Y
  res = varresult$res
  u = res

  # Compute terms ----
  J = compute_J(K, lag)
  Phi = red_ir(lag, K, A_comp, const, N)

  # Compute y_hat from VARMA representation ----
  sapply(1:N, function(t) {
    terms_to_sum = lapply(1:t, function(s) {
      w[t-s+1,k] %*% Theta[[s]][k,]
    })
  })

  return(Phi)

}
