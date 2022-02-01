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

  # Output:
  fitted_values <- fitted(deepvar_model)
  deepvar_model$fitted_values <- fitted_values$one_step_ahead
  if (deepvar_model$model_data$n_ahead > 1) {
    deepvar_model$all_fitted_values <- fitted_values$all
  }
  deepvar_model$residuals <- residuals(deepvar_model)

  return(deepvar_model)

}

#' @export
train <- function(deepvar_model,num_epochs=50) {
  UseMethod("train", deepvar_model)
}

## Predictions: ----
#' @export
fitted.deepvar_model <- function(deepvar_model, input_dl=NULL) {

  if (is.null(input_dl)) {
    input_dl_use <- deepvar_model$model_data$full_dl
  } else {
    input_dl_use <- input_dl
  }

  if (is.null(deepvar_model$fitted_values) | !is.null(input_dl)) {
    all_fitted <- lapply(
      1:length(deepvar_model$model_list),
      function(k) {
        ensemble <- deepvar_model$model_list[[k]]$ensemble
        preds <- lapply(
          1:length(ensemble),
          function(m) {
            rnn <- ensemble[[m]]
            i <- 1
            coro::loop(for (b in input_dl_use) {
              input <- b$X
              output <- rnn(input$to(device = getOption("deepvar.device")))
              preds <- as.matrix(output)
            })
            return(preds)
          }
        )
        y_hat <- Reduce(`+`, preds)/length(preds) # average over ensembles
        y_hat <- y_hat * deepvar_model$model_list[[k]]$train_dl$dataset$train_sd[k] + deepvar_model$model_list[[k]]$train_dl$dataset$train_mean[k]
        return(t(y_hat))
      }
    )
    names(all_fitted) <- deepvar_model$model_data$response_var_names
    one_step_ahead_fitted <- sapply(all_fitted, function(i) i[1,])
  } else {
    if (!is.null(deepvar_model$all_fitted_values)) {
      all_fitted <- deepvar_model$all_fitted_values
    } else {
      all_fitted <- deepvar_model$fitted_values
    }
    one_step_ahead_fitted <- deepvar_model$fitted_values
  }
  fitted_values <- list(
    one_step_ahead = one_step_ahead_fitted,
    all = all_fitted
  )
  return(fitted_values)
}

#' @export
uncertainty.deepvar_model <- function(deepvar_model, X=NULL) {
  return(NULL)
}

#' @export
uncertainty <- function(deepvar_model, X=NULL) {
  UseMethod("uncertainty", deepvar_model)
}

#' @export
residuals.deepvar_model <- function(deepvar_model) {

  if (is.null(deepvar_model$res)) {
    y_hat <- deepvar_model$fitted_values
    lags <- deepvar_model$model_data$lags
    n_ahead <- deepvar_model$model_data$n_ahead
    N <- deepvar_model$model_data$N
    res <- deepvar_model$model_data$data[(lags + n_ahead):N,] - y_hat
  } else {
    res <- deepvar_model$res
  }
  return(res)

}

#' @export
predict.deepvar_model <- function(deepvar_model, n.ahead=NULL) {

  if (is.null(n.ahead)) {
    n.ahead <- deepvar_model$model_data$n_ahead
    message(sprintf("Producing %i-step ahead predictions from end of sample in line with Deep VAR training.", n.ahead))
  }

  lags <- deepvar_model$model_data$lags
  N <- deepvar_model$model_data$N
  input_ds <- deepvar_model$model_data$data[(N - lags + 1):N,] |> as.matrix()
  input_dl <- input_ds |>
    dvar_input_data(lags = lags, train_mean = deepvar_model$model_data$train_mean, train_sd = deepvar_model$model_data$train_sd) |>
    torch::dataloader(batch_size = lags)
  preds <- fitted(deepvar_model, input_dl = input_dl)$all

  if (deepvar_model$model_data$n_ahead < n.ahead) {
    warning(
      sprintf(
        "Deep VAR was trained to predict only %i periods, not %i. Predictions beyond %i are computed recursively.",
        deepvar_model$model_data$n_ahead,
        n.ahead,
        deepvar_model$model_data$n_ahead
      )
    )
    for (t in 1:(n.ahead-unique(sapply(preds, length)))) {
      first_out_of_sample <- matrix(sapply(preds, function(pred) pred[t,]),1)
      input_ds <- rbind(input_ds, first_out_of_sample)
      input_ds <- input_ds[(nrow(input_ds) - lags + 1):nrow(input_ds),] # get past data
      input_dl <- input_ds |>
        dvar_input_data(lags = lags, train_mean = deepvar_model$model_data$train_mean, train_sd = deepvar_model$model_data$train_sd) |>
        torch::dataloader(batch_size = lags)
      recursive_preds <- fitted(deepvar_model, input_dl = input_dl)$all
      preds <- lapply(1:length(preds), function(k) rbind(preds[[k]], recursive_preds[[k]][lags,]))
    }
  }

  return(preds)
}

#' #' @export
#' prepare_predictors.deepvar_model <- function(deepvar_model, data) {
#'
#'   lags <- deepvar_model$model_data$lags
#'
#'   # Explanatory variables:
#'   X = as.matrix(
#'     data[
#'       (.N-(lags-1)):.N, # take last p rows
#'       sapply(
#'         0:(lags-1),
#'         function(lag) {
#'           data.table::shift(.SD, lag)
#'         }
#'       )
#'       ][.N,] # take last row of that
#'   )
#'
#'   X <- keras::array_reshape(X, dim = c(1,1,ncol(X)))
#'
#'   return(X)
#'
#' }
#'
#' #' @export
#' prepare_predictors <- function(deepvar_model, data) {
#'   UseMethod("prepare_predictors", deepvar_model)
#' }

