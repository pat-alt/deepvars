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
  predictions <- data.table::melt(
    data.table::data.table(y_hat),
    measure.vars=var_model$model_data$var_names
  )

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

## loss: ----
#' @export
residuals.var_model <- function(var_model) {
  res <- data.table::data.table(var_model$res)
  res[,date:=var_model$model_data$data[,date][1:(.N)]]
  res <- melt(res, id.vars="date")
  return(res)
}

#' @export
## Mean squared error (MSE): ----
mse.var_model <- function(var_model) {

  res <- residuals(var_model)
  mse <- res[,.(value=mean((value)^2)),by=variable]

  return(mse)
}

#' @export
mse <- function(var_model) {
  UseMethod("mse", var_model)
}

#' @export
## Root mean squared error (RMSE): ----
rmse.var_model <- function(var_model) {

  res <- residuals(var_model)
  rmse <- res[,.(value=sqrt(mean((value)^2))),by=variable]

  return(rmse)
}

#' @export
rmse <- function(var_model) {
  UseMethod("rmse", var_model)
}

## Cumulative loss: ----
#' @export
cum_loss.var_model <- function(var_model) {

  res <- residuals(var_model)
  cum_loss <- list(cum_loss = res[,.(date=date, value=cumsum(value^2)),by=variable])
  class(cum_loss) <- "cum_loss"
  return(cum_loss)
}

#' @export
cum_loss <- function(var_model) {
  UseMethod("cum_loss", var_model)
}

## Forecasting: ----
#' @export
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

