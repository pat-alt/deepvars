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
      foreach::foreach(m = 1:deepvar_model$size_ensemble) %dopar% {
        message(sprintf("Training model %i for response %i",m,k))
        deepvar_model$model_list[[k]]$ensemble[[m]] <- forward_rnn(
          rnn = deepvar_model$model_list[[k]]$ensemble[[m]],
          train_dl = deepvar_model$model_list[[k]]$train_dl,
          valid_dl = deepvar_model$model_list[[k]]$valid_dl,
          loss = loss,
          optim_fun = optim_fun,
          optim_args = optim_args,
          num_epochs = num_epochs,
          verbose = verbose,
          tau = tau,
          patience = patience,
          show_progress = show_progress
        )
      }
  } else {
    # Train ensembles:
    foreach::foreach(k = 1:length(deepvar_model$model_list)) %:%
      foreach::foreach(m = 1:deepvar_model$size_ensemble) %do% {
        message(sprintf("Training model %i for response %i",m,k))
        deepvar_model$model_list[[k]]$ensemble[[m]] <- forward_rnn(
          rnn = deepvar_model$model_list[[k]]$ensemble[[m]],
          train_dl = deepvar_model$model_list[[k]]$train_dl,
          valid_dl = deepvar_model$model_list[[k]]$valid_dl,
          loss = loss,
          optim_fun = optim_fun,
          optim_args = optim_args,
          num_epochs = num_epochs,
          verbose = verbose,
          tau = tau,
          patience = patience,
          show_progress = show_progress
        )
      }
  }

  # Output:
  fitted_values <- fitted(deepvar_model)
  deepvar_model$fitted_values <- fitted_values$one_step_ahead
  if (deepvar_model$model_data$n_ahead > 1) {
    deepvar_model$all_fitted_values <- fitted_values$all
  }
  deepvar_model$res <- residuals(deepvar_model)

  return(deepvar_model)

}

#' @export
train <- function(deepvar_model,num_epochs=50) {
  UseMethod("train", deepvar_model)
}

## Predictions: ----
#' @export
fitted.deepvar_model <- function(deepvar_model, input_dl=NULL) {

  device <- torch::torch_device(getOption("deepvar.device"))

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
              output <- rnn(input$to(device = device))
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
uncertainty.deepvar_model <- function(deepvar_model) {
  uncertainty <- matrix(
    rep(NA,deepvar_model$model_data$K*deepvar_model$model_data$N),
    ncol = deepvar_model$model_data$K,
    byrow = TRUE
  )
  colnames(uncertainty) <- deepvar_model$model_data$response_var_names
  return(uncertainty)
}

#' @export
uncertainty <- function(deepvar_model) {
  UseMethod("uncertainty", deepvar_model)
}

#' @export
residuals.deepvar_model <- function(deepvar_model) {

  if (is.null(deepvar_model$res)) {
    y_hat <- deepvar_model$fitted_values
    lags <- deepvar_model$model_data$lags
    n_ahead <- deepvar_model$model_data$n_ahead
    N <- deepvar_model$model_data$N
    res <- deepvar_model$model_data$data[(lags + 1):N,] - y_hat
  } else {
    res <- deepvar_model$res
  }
  return(res)

}

#' @export
predict.deepvar_model <- function(deepvar_model, n.ahead=NULL, input_ds=NULL) {

  if (is.null(n.ahead)) {
    n.ahead <- deepvar_model$model_data$n_ahead
    message(sprintf("Producing %i-step ahead predictions from end of sample in line with Deep VAR training.", n.ahead))
  }

  lags <- deepvar_model$model_data$lags
  N <- deepvar_model$model_data$N
  if (is.null(input_ds)) {
    input_ds <- deepvar_model$model_data$data[(N - lags + 1):N,] |> as.matrix()
  } else {
    if (nrow(input_ds) < deepvar_model$model_data$lags) stop(
      sprintf(
        "Input data has less observations (%i) than chosen lags (%i). Aborting.",
        nrow(input_ds),
        deepvar_model$model_data$lags
      )
    )
  }
  input_dl <- input_ds |>
    deepvar_input_data(lags = lags, train_mean = deepvar_model$model_data$train_mean, train_sd = deepvar_model$model_data$train_sd) |>
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
        deepvar_input_data(lags = lags, train_mean = deepvar_model$model_data$train_mean, train_sd = deepvar_model$model_data$train_sd) |>
        torch::dataloader(batch_size = lags)
      recursive_preds <- fitted(deepvar_model, input_dl = input_dl)$all
      preds <- lapply(1:length(preds), function(k) rbind(preds[[k]], recursive_preds[[k]][lags,]))
      names(preds) <- deepvar_model$model_data$response_var_names
    }
  }

  # Turn into matrix:
  preds <- list2DF(preds) |> as.matrix()

  # Return predictions:
  prediction <- list(
    prediction = preds,
    uncertainty = uncertainty(deepvar_model),
    model_data = deepvar_model$model_data
  )
  class(prediction) <- "prediction"

  return(prediction)
}

