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
  list2env(varresult, envir = environment())
  list2env(varresult$model_data, envir = environment())

  # Compute terms ----
  J = compute_J(K, lags)
  Phi = red_ir(lags, K, A_comp, const, N)

  # Compute y_hat from VARMA representation ----
  sapply(1:N, function(t) {
    terms_to_sum = lapply(1:t, function(s) {
      w[t-s+1,k] %*% Theta[[s]][k,]
    })
  })

  return(Phi)

}
