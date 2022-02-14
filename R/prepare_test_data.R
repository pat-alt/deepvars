#' @export
prepare_test_data <- function(test_data, lags) {
  X_test <- prepare_var_data(test_data, lags)$X
  y_test <- prepare_var_data(test_data, lags)$y
  return(
    list(
      X_test = X_test,
      y_test = y_test
    )
  )
}
