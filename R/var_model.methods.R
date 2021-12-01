#' @export
print.var_model <- function(var_model) {
  print(knitr::kable(var_model$coeff_tidy))
}

## Predictions: ----
#' Fitted values
#'
#' @param var_model
#' @param X
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer
fitted.var_model <- function(var_model, X=NULL) {
  if (is.null(X)) {
    y_hat <- var_model$y_hat
  } else {
    y_hat <- tryCatch(
      X %*% var_model$A,
      error = function(e) {
        return(cbind(1,X) %*% var_model$A)
      }
    )
  }
  return(y_hat)
}

#' @export
uncertainty.var_model <- function(var_model, X=NULL) {
  if (is.null(X)) {
    uncertainty <- matrix(
      rep(sqrt(diag(var_model$Sigma_res)),var_model$model_data$N),
      ncol=var_model$model_data$K,
      byrow = TRUE
    )
    colnames(uncertainty) <- var_model$model_data$var_names
  } else {
    # PLACEHOLDER:
    uncertainty <- matrix(
      rep(sqrt(diag(var_model$Sigma_res)),nrow(X)),
      ncol=var_model$model_data$K,
      byrow = TRUE
    )
    colnames(uncertainty) <- var_model$model_data$var_names
  }
  return(uncertainty)
}

#' @export
uncertainty <- function(var_model, X=NULL) {
  UseMethod("uncertainty", var_model)
}


#' @export
residuals.var_model <- function(var_model, X=NULL, y=NULL) {

  new_data <- new_data_supplied(X=X,y=y)

  if (new_data | is.null(var_model$res)) {
    y_hat <- fitted(var_model, X)
    res <- y - y_hat
  } else {
    res <- var_model$res
  }

  return(res)

}

#' @export
prepare_predictors.var_model <- function(var_model, data) {

  lags <- var_model$model_data$lags

  # Explanatory variables:
  X = as.matrix(
    data[
      (.N-(lags-1)):.N, # take last p rows
      sapply(
        0:(lags-1),
        function(lag) {
          data.table::shift(.SD, lag)
        }
      )
      ][.N,] # take last row of that
  )

  return(X)

}

#' @export
prepare_predictors <- function(var_model, data) {
  UseMethod("prepare_predictors", var_model)
}
