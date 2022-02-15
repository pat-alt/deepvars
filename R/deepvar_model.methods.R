#' Title
#'
#' @param deepvar_model
#' @param verbose
#' @param ...
#'
#' @importFrom foreach `%dopar%` `%do%`
#' @importFrom keras `%>%`
#'
#' @return
#' @export
#'
#' @examples
fit.deepvar_model <- function(deepvar_model,verbose=0,...) {

  K <- deepvar_model$model_data$K
  X_train <- deepvar_model$model_data$X
  y_train <- deepvar_model$model_data$y

  # Train deep ensembles:
  for (k in 1:K) {

    ensemble <- deepvar_model$model_list[[k]]
    M <- length(ensemble)

    foreach::foreach(m=1:M,.packages=c("keras")) %do% {

      message(sprintf("Training model %i for response %i",m,k))

      ensemble[[m]] %>%
        keras::fit(
          x = X_train, y = y_train[,,k],
          verbose = verbose,
          ...
        )

    }

  }

  # Output:
  deepvar_model$X_train <- X_train
  deepvar_model$y_train <- y_train

  return(deepvar_model)

}

fit <- function(deepvar_model,verbose=0,...) {
  UseMethod("fit", deepvar_model)
}

## Predictions: ----
#' @export
posterior_predictive.deepvar_model <- function(deepvar_model, X=NULL) {

  if (is.null(X) & !is.null(deepvar_model$y_hat)) {
    y_hat <- deepvar_model$y_hat
  } else {

    # Preprocessing:
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

    # Compute fitted values:
    fitted <- lapply(
      1:length(deepvar_model$model_list),
      function(k) {

        ensemble <- deepvar_model$model_list[[k]]
        M <- length(ensemble)
        y_hat_li <- list()
        sd_li <- list()

        # Compute fitted
        foreach::foreach(m=1:M,.packages=c("keras")) %do% {
          fitted <- ensemble[[m]](X)
          y_hat <- as.numeric(fitted %>% tfprobability::tfd_mean())
          sd <- as.numeric(fitted %>% tfprobability::tfd_stddev())
          # Rescale data:
          y_hat <- unlist(invert_scaling(y_hat, deepvar_model$model_data, k=k))
          sd <- unlist(invert_scaling(sd, deepvar_model$model_data, k=k))
          # Record:
          y_hat_li[[m]] <- y_hat
          sd_li[[m]] <- sd
        }

        y_hat <- rowMeans(do.call(cbind, y_hat_li)) # posterior mean
        sd <- rowMeans(do.call(cbind, lapply(1:M, function(m) {(sd_li[[m]]^2 + y_hat_li[[m]]^2) - y_hat^2})))

        return(list(y_hat=y_hat, sd=sd))
      }
    )
    # Posterior mean:
    y_hat <- matrix(sapply(fitted, function(i) i$y_hat), ncol = deepvar_model$model_data$K)
    rownames(y_hat) <- NULL
    colnames(y_hat) <- deepvar_model$model_data$var_names
    # Posterior variance:
    sd <- matrix(sapply(fitted, function(i) i$sd), ncol = deepvar_model$model_data$K)
    rownames(sd) <- NULL
    colnames(sd) <- deepvar_model$model_data$var_names
  }

  return(list(mean=y_hat, sd=sd))
}

#' @export
posterior_predictive <- function(deepvar_model, X=NULL) {
  UseMethod("posterior_predictive", deepvar_model)
}

#' @export
fitted.deepvar_model <- function(deepvar_model, X=NULL) {
  if (is.null(X)) {
    y_hat <- deepvar_model$posterior_predictive$mean
  } else {
    y_hat <- posterior_predictive(deepvar_model, X)$mean
  }
  return(y_hat)
}

#' @export
uncertainty.deepvar_model <- function(deepvar_model, X=NULL) {
  if (is.null(X)) {
    uncertainty <- deepvar_model$posterior_predictive$sd
  } else {
    uncertainty <- posterior_predictive(deepvar_model, X)$sd
  }
  return(uncertainty)
}

#' @export
uncertainty <- function(deepvar_model, X=NULL) {
  UseMethod("uncertainty", deepvar_model)
}

#' @export
residuals.deepvar_model <- function(deepvar_model, X=NULL, y=NULL) {

  new_data <- new_data_supplied(X=X,y=y)

  if (new_data | is.null(deepvar_model$res)) {
    if (!new_data) {
      # If no new data is supplied, training outputs are re-scaled:
      X <- deepvar_model$X_train
      y <- deepvar_model$y_train
      y <- keras::array_reshape(y, dim=c(dim(y)[1],dim(y)[3]))
      y <- invert_scaling(y, deepvar_model$model_data)
    }
    y_hat <- fitted(deepvar_model, X=X) # automatically rescaled
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

