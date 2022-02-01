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
    stop("Sorry, running deep ensemble in parallel is not yet supported.")
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

  deepvar_model$fitted_values <- fitted(deepvar_model)

  return(deepvar_model)

}

#' @export
train <- function(deepvar_model,num_epochs=50) {
  UseMethod("train", deepvar_model)
}

## Predictions: ----
#' @export
fitted.deepvar_model <- function(deepvar_model) {

  if (is.null(deepvar_model$fitted_values)) {
    fitted_values <- lapply(
      1:length(deepvar_model$model_list),
      function(k) {
        full_dl <- deepvar_model$model_data$full_dl[[k]]
        ensemble <- deepvar_model$model_list[[k]]$ensemble
        preds <- lapply(
          1:length(ensemble),
          function(m) {
            rnn <- ensemble[[m]]
            i <- 1
            coro::loop(for (b in full_dl) {
              input <- b$X
              output <- rnn(input$to(device = getOption("deepvar.device")))
              preds <- as.matrix(output)
            })
            return(preds)
          }
        )
        y_hat <- Reduce(`+`, preds)/length(preds) # average over ensembles
        y_hat <- y_hat * deepvar_model$model_list[[k]]$train_dl$dataset$train_sd[k] + deepvar_model$model_list[[k]]$train_dl$dataset$train_mean[k]
        return(y_hat)
      }
    )
  } else {
    fitted_values <- deepvar_model$fitted_values
  }
  return(fitted_values)
}

#' #' @export
#' fitted <- function(deepvar_model) {
#'   UseMethod("fitted", deepvar_model)
#' }

#' #' @export
#' fitted.deepvar_model <- function(deepvar_model, X=NULL) {
#'   if (is.null(X)) {
#'     y_hat <- deepvar_model$posterior_predictive$mean
#'   } else {
#'     y_hat <- posterior_predictive(deepvar_model, X)$mean
#'   }
#'   return(y_hat)
#' }

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

