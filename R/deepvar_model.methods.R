#' Title
#'
#' @param deepvar_model
#' @param num_epochs
#'
#' @return
#' @export
#'
#' @importFrom foreach `%dopar%` `%:%` `%do%`
#'
#' @examples
train.deepvar_model <- function(deepvar_model,num_epochs=50) {

  # Setup:
  list2env(getOption("deepvar.train"), envir = environment()) # load training options
  verbose <- getOption("deepvar.verbose")

  if (parallelize) {
    # Set up cluster
    no_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(no_cores, type = "FORK")
    doParallel::registerDoParallel(cl)
    # Train ensembles in parallel:
    foreach::foreach(k = 1:length(deepvar_model$model_list)) %:%
      foreach::foreach(m = 1:size_ensemble) %dopar% {
        if (verbose) {
          message(sprintf("Training model %i for response %i",m,k))
        }
        forward_rnn(
          rnn = deepvar_model$model_list[[k]]$ensemble[[m]],
          train_dl = deepvar_model$model_list[[k]]$train_dl,
          valid_dl = deepvar_model$model_list[[k]]$valid_dl,
          loss = loss,
          optim_fun = optim_fun,
          optim_args = optim_args,
          num_epochs = num_epochs,
          verbose = verbose,
          tau = tau
        )
      }
  } else {
    # Train ensembles:
    foreach::foreach(k = 1:length(deepvar_model$model_list)) %:%
      foreach::foreach(m = 1:size_ensemble) %do% {
        if (verbose) {
          message(sprintf("Training model %i for response %i",m,k))
        }
        forward_rnn(
          rnn = deepvar_model$model_list[[k]]$ensemble[[m]],
          train_dl = deepvar_model$model_list[[k]]$train_dl,
          valid_dl = deepvar_model$model_list[[k]]$valid_dl,
          loss = loss,
          optim_fun = optim_fun,
          optim_args = optim_args,
          num_epochs = num_epochs,
          verbose = verbose,
          tau = tau
        )
      }
  }

  return(deepvar_model)

}

#' @export
train <- function(deepvar_model,num_epochs=50) {
  UseMethod("train", deepvar_model)
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
        mod <- deepvar_model$model_list[[k]]
        fitted <- mod(X)
        y_hat <- as.numeric(fitted %>% tfprobability::tfd_mean())
        sd <- as.numeric(fitted %>% tfprobability::tfd_stddev())
        # Rescale data:
        y_hat <- invert_scaling(y_hat, deepvar_model$model_data, k=k)
        sd <- invert_scaling(sd, deepvar_model$model_data, k=k)
        return(list(y_hat=unlist(y_hat), sd=unlist(sd)))
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

