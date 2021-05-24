# Fit model: ----
fit.deepvar_model <- function(deepvar_model,verbose=0,...) {

  K <- deepvar_model$model_data$K
  X_train <- deepvar_model$model_data$X
  y_train <- deepvar_model$model_data$y

  # Fit models:
  fitted_models <- lapply(
    1:K,
    function(k) {
      history <- deepvar_model$model_list[[k]] %>%
        keras::fit(
          x = X_train, y = y_train[,,k],
          verbose = verbose,
          ...
        )
      list(
        model = deepvar_model$model_list[[k]],
        history = history
      )
    }
  )

  # Output:
  deepvar_model$model_list <- lapply(fitted_models, function(fitted_model) fitted_model[["model"]]) # update model list
  deepvar_model$model_histories <- lapply(fitted_models, function(fitted_model) fitted_model[["history"]]) # extract history
  deepvar_model$X_train <- X_train
  deepvar_model$y_train <- y_train

  return(deepvar_model)

}

fit <- function(deepvar_model,...) {
  UseMethod("fit", deepvar_model)
}

## Predictions: ----
#' @export
fitted.deepvar_model <- function(deepvar_model, X=NULL) {

  if (is.null(X) & !is.null(deepvar_model$y_hat)) {
    y_hat <- deepvar_model$y_hat
  } else {
    if (is.null(X)) {
      X <- deepvar_model$X_train
    }
    y_hat <- sapply(
      1:length(deepvar_model$model_list),
      function(k) {
        mod <- deepvar_model$model_list[[k]]
        y_hat <- mod %>%
          stats::predict(X)
        y_hat <- invert_scaling(y_hat, deepvar_model$model_data, k=k)
        return(unlist(y_hat))
      }
    )
    rownames(y_hat) <- NULL
    colnames(y_hat) <- deepvar_model$model_data$var_names
  }

  return(y_hat)
}

#' @export
residuals.deepvar_model <- function(deepvar_model) {

  y_hat <- fitted(deepvar_model)
  y <- keras::array_reshape(deepvar_model$y_train, dim=c(dim(deepvar_model$y_train)[1],dim(deepvar_model$y_train)[3]))
  y <- invert_scaling(y, deepvar_model$model_data)
  res <- y - y_hat

  return(res)

}
