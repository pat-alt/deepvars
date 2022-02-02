#' Deep VAR
#'
#' @param data
#' @param lags
#' @param response
#' @param size_ensemble
#' @param num_epochs
#' @param n_ahead
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer
deepvareg <- function(
  train_ds,
  valid_ds,
  lags,
  response=NULL,
  n_ahead=1,
  size_ensemble=1,
  num_epochs=100
) {

  # Prepare data:
  deepvar_data <- prepare_deepvar_data(train_ds=train_ds, valid_ds=valid_ds, lags=lags, n_ahead=n_ahead, response=response)
  # Prepare model:
  deepvar_model <- prepare_deepvar_model(deepvar_data, size_ensemble)
  # Train the model:
  deepvar_model <- train(deepvar_model, num_epochs)

  class(deepvar_model) <- c("deepvar_model", "model") # assign broader model class

  return(deepvar_model)

}

