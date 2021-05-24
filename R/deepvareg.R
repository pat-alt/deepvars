#' Deep VAR
#'
#' @param data
#' @param lags
#' @param n_units
#' @param horizon
#' @param type
#'
#' @importFrom magrittr %>%
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer
deepvareg <- function(data, lags=1, n_units=50, horizon=1, type="var", verbose=0, ...) {
  # Prepare data:
  deepvar_data <- prepare_deepvar_data(data, lags, horizon, type)

  # Prepare model:
  deepvar_model <- prepare_deepvar_model(deepvar_data, n_units=n_units)
  # K <- deepvar_data$K
  # dim_input <- dim(deepvar_data$X)[2:3]
  # model_list <- lapply(
  #   1:K,
  #   function(k) {
  #     model <- keras::keras_model_sequential() %>%
  #       keras::layer_lstm(units = n_units, input_shape = dim_input) %>%
  #       keras::layer_dropout(0.5) %>%
  #       keras::layer_dense(units = 1)
  #     model %>%
  #       keras::compile(
  #         loss = "mae",
  #         optimizer = "adam"
  #       )
  #     return(model)
  #   }
  # )
  #
  # deepvar_model <- list(
  #   model_list = model_list,
  #   model_data = deepvar_data
  # )

  # Fit the model:
  deepvar_model <- fit(deepvar_model, verbose=verbose, ...)
  # X_train <- deepvar_model$model_data$X
  # y_train <- deepvar_model$model_data$y
  # fitted_models <- lapply(
  #   1:K,
  #   function(k) {
  #     history <- deepvar_model$model_list[[k]] %>%
  #       keras::fit(
  #         x = X_train, y = y_train[,,k],
  #         verbose = verbose,
  #         ...
  #       )
  #     list(
  #       model = deepvar_model$model_list[[k]],
  #       history = history
  #     )
  #   }
  # )
  # deepvar_model$X_train <- X_train
  # deepvar_model$y_train <- y_train

  # Fitted values:
  X_train <- deepvar_model$X_train
  y_train <- deepvar_model$y_train
  y_hat <- sapply(
    1:length(deepvar_model$model_list),
    function(k) {
      mod <- deepvar_model$model_list[[k]]
      y_hat <- mod %>%
        stats::predict(X_train)
      y_hat <- invert_scaling(y_hat, deepvar_model$model_data, k=k)
      return(unlist(y_hat))
    }
  )
  rownames(y_hat) <- NULL
  colnames(y_hat) <- deepvar_model$model_data$var_names
  deepvar_model$y_hat <- y_hat

  # Residuals:
  deepvar_model$res <- keras::array_reshape(y_train, dim = c(dim(y_train)[1],dim(y_train)[3])) - y_hat

  class(deepvar_model) <- c("deepvar_model", "dvars_model")

  return(deepvar_model)
}

