## Cumulative loss: ----
#' @export
cum_loss.model <- function(model) {
  cum_loss <- sapply(1:ncol(model$res), function(k) cumsum(model$res[,k]**2))
  colnames(cum_loss) <- model$model_data$response_var_names
  return(cum_loss)
}

#' @export
cum_loss <- function(model) {
  UseMethod("cum_loss", model)
}
