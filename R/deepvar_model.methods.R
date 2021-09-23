# Fit model: ----
#' @importFrom keras `%>%`
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
    if (length(dim(X))<3) {
      # ! If new data is not 3D tensor, assume that unscaled 2D tensor was supplied !
      # Get rid of constant:
      if (all(X[,1]==1)) {
        X <- X[,-1]
      }
      # Apply scaling:
      scaler <- deepvar_model$model_data$scaler
      lags <- deepvar_model$model_data$lags
      K <- deepvar_model$model_data$K
      X <- apply_scaler_from_training(X, scaler, lags, K)
      # Reshape:
      X <- keras::array_reshape(X, dim=c(dim(X)[1],1,dim(X)[2]))

    }
    y_hat <- matrix(
      sapply(
        1:length(deepvar_model$model_list),
        function(k) {
          mod <- deepvar_model$model_list[[k]]
          y_hat <- mod %>%
            stats::predict(X)
          # Rescale data:
          y_hat <- invert_scaling(y_hat, deepvar_model$model_data, k=k)
          return(unlist(y_hat))
        }
      ),
      ncol = deepvar_model$model_data$K
    )
    rownames(y_hat) <- NULL
    colnames(y_hat) <- deepvar_model$model_data$var_names
  }

  return(y_hat)
}

#' @export
residuals.deepvar_model <- function(deepvar_model, X=NULL, y=NULL) {

  new_data <- new_data_supplied(X=X,y=y)

  if (new_data | is.null(deepvar_model$res)) {
    if (!new_data) {
      X <- deepvar_model$X_train
      y <- deepvar_model$y_train
    }
    y_hat <- fitted(deepvar_model, X=X)
    y <- keras::array_reshape(y, dim=c(dim(y)[1],dim(y)[3]))
    y <- invert_scaling(y, deepvar_model$model_data)
    res <- y - y_hat
  } else {
    res <- deepvar_model$res
  }

  return(res)

}

#' @export
prepare_predictors.deepvar_model <- function(deepvar_model, data) {

  lags <- deepvar_model$model_data$lags

  # Explanatory variables:
  X = as.matrix(
    data[
      (.N-(lags-1)):.N, # take last p rows
      sapply(
        0:(lags-1),
        function(lag) {
          data.table::shift(.SD, lag)
        }
      )
      ][.N,] # take last row of that
  )

  X <- keras::array_reshape(X, dim = c(1,1,ncol(X)))

  return(X)

}

#' @export
prepare_predictors <- function(deepvar_model, data) {
  UseMethod("prepare_predictors", deepvar_model)
}

