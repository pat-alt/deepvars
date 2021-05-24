#' @export
prepare_deepvar_data <- function(data, lags=1, horizon=1, type="var") {

  if (type=="var") {
    var_data <- prepare_var_data(data, lags = lags, standardize = TRUE)
    deepvar_data <- var2deepvar(var_data)
  } else if (type=="standard") {
    deepvar_data <- deepvar_standard(data, lags=lags, horizon=1)
  }

  return(deepvar_data)

}
