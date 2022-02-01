#' @export
print.var_model <- function(var_model) {
  print(knitr::kable(var_model$coeff_tidy))
}

## Predictions: ----
#' Fitted values
#'
#' @param var_model
#' @param X
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer
fitted.var_model <- function(var_model) {
  return(var_model$y_hat)
}

#' @export
uncertainty.var_model <- function(var_model) {

  uncertainty <- matrix(
    rep(sqrt(diag(var_model$Sigma_res)),var_model$model_data$N),
    ncol=var_model$model_data$K,
    byrow = TRUE
  )
  colnames(uncertainty) <- var_model$model_data$var_names

  return(uncertainty)
}

#' @export
uncertainty <- function(var_model, X=NULL) {
  UseMethod("uncertainty", var_model)
}

#' @export
residuals.var_model <- function(var_model) {
  return(var_model$res)
}

# Predictions: ----
#' @export
predict.var_model <- function(var_model, n.ahead = 10) {

  lags <- var_model$model_data$lags
  K <- var_model$model_data$K
  N <- var_model$model_data$N + lags
  input_ds <- var_model$model_data$data[(N-lags+1):N] |> as.matrix()
  preds <- matrix(rep(0,K*n.ahead),n.ahead)

  # Forecast recursively:
  for (t in 1:n.ahead) {
    X <- embed(input_ds, lags)
    preds[t,] <- fitted(var_model, X)
    input_ds <- rbind(input_ds, preds[t,])[-1,]
  }

  # Return predictions:
  predictions <- list(
    predictions = preds,
    uncertainty = NULL
  )
  class(predictions) <- "predictions"
  return(predictions)
}
