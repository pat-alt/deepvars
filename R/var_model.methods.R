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
fitted <- function(var_model, X=NULL) {
  UseMethod("fitted", var_model)
}

#' @export
predict.var_model <- function(var_model, X=NULL) {
  # Compute predictions:
  y_hat <- fitted(var_model, X)
  predictions <- data.table::melt(data.table::data.table(y_hat), measure.vars=var_model$model_data$var_names)

  # Return predictions:
  predictions <- list(
    predictions = predictions,
    X = X,
    model = var_model
  )
  class(predictions) <- "predictions"
  return(predictions)
}

#' @export
predict <- function(var_model, X=NULL) {
  UseMethod("predict", var_model)
}

#' @export
## Mean squared error (MSE): ----
mse.var_model <- function(var_model,X=NULL,y=NULL) {

  # Has X, y been supplied?
  if (is.null(X) & is.null(y)) {
    X <- var_model$X_train
    y <- var_model$y_train
  }

  # Set up:
  model_data <- var_model$model_data

  # Predictions:
  pred <- predict(var_model, X=X)
  y_hat <- pred$predictions
  y_hat[,type:="y_hat"]

  # Observed values:
  y_true <- data.table::data.table(y)
  y_true[,type:="y_true"]
  y_true <- melt(y_true, id.vars = "type")

  # Compute MSE:
  mse <- rbind(y_hat, y_true)
  mse[,id:=1:(.N),by=.(variable, type)]
  mse <- dcast(mse, variable + id ~ type, value.var="value")
  mse <- mse[,.(mse=mean((y_hat-y_true)^2)),by=variable]

  return(mse)
}

#' @export
mse <- function(var_model,X=NULL,y=NULL) {
  UseMethod("mse", var_model)
}

#' @export
## Root mean squared error (RMSE): ----
rmse.var_model <- function(var_model,X=NULL,y=NULL) {

  # Has X, y been supplied?
  if (is.null(X) & is.null(y)) {
    X <- var_model$X_train
    y <- var_model$y_train
  }

  # RMSE:
  mse <- mse(var_model, X=X, y=y)
  rmse <- mse[,.(rmse=sqrt(mse)),by=variable]

  return(rmse)
}

#' @export
rmse <- function(var_model,X=NULL,y=NULL) {
  UseMethod("rmse", var_model)
}

#' @export
## Forecasting: ----
forecast.var_model <- function(var_model, n.ahead=1) {

  # Set up:
  var_names <- var_model$model_data$var_names
  lags <- var_model$model_data$lags
  sample <- copy(var_model$model_data$data)
  if (!"date" %in% names(sample)) {
    sample[,date:=1:.N]
  }
  fcst <- data.table()
  data <- rbind(sample, fcst)
  counter <- 1
  increment_date <- ifelse(
    sample[,class(date)=="Date"], round(sample[,mean(diff(date))]), 1
  )

  # Forecast recursively:
  while(counter <= n.ahead) {
    X <- prepare_predictors(data[,.SD,.SDcols=var_names],lags)
    y_hat <- predict(var_model, X)

    # Update
    fcst_t <- dcast(y_hat$predictions, .~variable)[,-1]
    fcst_t[,date:=data[.N,date+increment_date]]
    fcst <- rbind(fcst, fcst_t)
    data <- rbind(data, fcst_t)
    counter <- counter + 1
  }
  setcolorder(fcst, "date")

  # Return:
  fcst <- list(
    fcst = fcst,
    model_data = var_model$model_data
  )
  class(fcst) <- "forecast"

  return(fcst)

}

#' @export
forecast <- function(var_model, n.ahead=1) {
  UseMethod("forecast", var_model)
}

