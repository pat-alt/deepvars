#' Deep VAR
#'
#' @param data
#' @param lags
#' @param n_units
#' @param horizon
#' @param type
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer
deepvareg <- function(
  data,
  lags=1,
  num_units=50,
  num_layers=2,
  p_drop_out=0.5,
  horizon=1,
  type="var",
  verbose=0,
  ...
) {

  # Prepare data:
  deepvar_data <- prepare_deepvar_data(data, lags, horizon, type)
  # Prepare model:
  deepvar_model <- prepare_deepvar_model(
    deepvar_data,
    num_units = num_units,
    num_layers = num_layers,
    p_drop_out = p_drop_out
  )
  # Fit the model:
  deepvar_model <- fit(deepvar_model, verbose=verbose, ...)
  # Posterior predictive:
  deepvar_model$posterior_predictive <- posterior_predictive(deepvar_model)
  # Fitted values:
  deepvar_model$y_hat <- deepvar_model$posterior_predictive$mean
  # Residuals:
  deepvar_model$res <- residuals(deepvar_model)

  class(deepvar_model) <- c("deepvar_model", "dvars_model") # assign broader model class

  return(deepvar_model)

}

