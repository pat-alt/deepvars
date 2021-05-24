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
  # Fit the model:
  deepvar_model <- fit(deepvar_model, verbose=verbose, ...)
  # Fitted values:
  deepvar_model$y_hat <- fitted(deepvar_model)
  # Residuals:
  deepvar_model$res <- residuals(deepvar_model)

  class(deepvar_model) <- c("deepvar_model", "dvars_model") # assign broader model class

  return(deepvar_model)

}

