## Predictions: ----
#' @export
fitted.deepvar_model <- function(deepvar_model, X=NULL) {

  if (is.null(X)) {
    y_hat <- deepvar_model$y_hat
  } else {
    y_hat <- sapply(
      1:length(deepvar_model$model_list),
      function(k) {
        mod <- deepvar_model$model_list[[k]]
        y_hat <- mod %>%
          stats::predict(X)
        y_hat <- invert_scaling(y_hat, model_data, k=k)
        return(y_hat$value)
      }
    )
  }

  return(y_hat)
}
