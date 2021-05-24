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

  if (is.null(X)) {
    y_hat <- deepvar_model$y_hat
  } else {
    y_hat <- sapply(
      1:length(deepvar_model$model_list),
      function(k) {
        mod <- deepvar_model$model_list[[k]]
        y_hat <- mod %>%
          stats::predict(X)
        y_hat <- invert_scaling(y_hat, model_data, k=k)
        return(y_hat$value)
      }
    )
  }

  return(y_hat)
}
