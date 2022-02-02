#' @export
prepare_last_as_input.model <- function(model, input_ds) {
  N <- nrow(input_ds)
  lags <- model$model_data$lags
  return(input_ds[(N - lags + 1):N,] |> as.matrix())
}

#' @export
prepare_last_as_input <- function(model, input_ds) {
  UseMethod("prepare_last_as_input", model)
}


## Cumulative loss: ----
#' @export
cum_loss.model <- function(model, input_ds=NULL) {

  if (is.null(input_ds)) {
    res <- model$res
  } else {
    res <- residuals(model, input_ds=input_ds)
  }
  cum_loss <- sapply(1:ncol(res), function(k) cumsum(res[,k]**2))
  colnames(cum_loss) <- model$model_data$response_var_names

  return(cum_loss)
}

#' @export
cum_loss <- function(model, input_ds=NULL) {
  UseMethod("cum_loss", model)
}
