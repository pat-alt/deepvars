#' @export
print.var_model <- function(var_model) {
  print(knitr::kable(var_model$coeff_tidy))
}

#' @export
fitted.var_model <- function(var_model, input_ds=NULL) {

  if (is.null(input_ds)) {
    input_ds_use <- deepvar_model$model_data$full_dl
  } else {
    input_ds_use <- input_ds
  }

  if (is.null(deepvar_model$fitted_values) | !is.null(input_ds)) {
    y_hat <- tryCatch(
      input_ds %*% var_model$A,
      error = function(e) {
        return(cbind(1,input_ds) %*% var_model$A)
      }
    )
  } else {
    y_hat <- var_model$y_hat
  }

  return(y_hat)
}

#' @export
uncertainty.var_model <- function(var_model) {

  uncertainty <- matrix(
    rep(sqrt(diag(var_model$Sigma_res)),var_model$model_data$N),
    ncol = var_model$model_data$K,
    byrow = TRUE
  )
  colnames(uncertainty) <- var_model$model_data$var_names
  return(uncertainty)
}

#' @export
uncertainty <- function(var_model) {
  UseMethod("uncertainty", var_model)
}

#' @export
residuals.var_model <- function(var_model) {
  return(var_model$res)
}

# Predictions: ----
#' @export
predict.var_model <- function(var_model, n.ahead = 10, input_ds=NULL) {

  lags <- var_model$model_data$lags
  K <- var_model$model_data$K
  N <- var_model$model_data$N + lags
  if (is.null(input_ds)) {
    input_ds <- var_model$model_data$data[(N - lags + 1):N] |> as.matrix()
  } else {
    if (nrow(input_ds) < deepvar_model$model_data$lags) stop(
      sprintf(
        "Input data has less observations (%i) than chosen lags (%i). Aborting.",
        nrow(input_ds),
        deepvar_model$model_data$lags
      )
    )
  }
  preds <- matrix(rep(0,K*n.ahead),n.ahead)

  # Forecast recursively:
  for (t in 1:n.ahead) {
    X <- embed(input_ds, lags)
    preds[t,] <- fitted(var_model, input_ds = X)
    input_ds <- rbind(input_ds, preds[t,])[-1,]
  }

  colnames(preds) <- var_model$model_data$var_names

  # Return predictions:
  prediction <- list(
    prediction = preds,
    uncertainty = uncertainty(var_model),
    model_data = var_model$model_data
  )
  class(prediction) <- "prediction"
  return(prediction)
}
