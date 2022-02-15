# Predictions: ----
#' @export
predict.dvars_model <- function(dvars_model, X=NULL) {
  # Compute predictions:
  y_hat <- fitted(dvars_model, X)
  predictions <- data.table::melt(
    data.table::data.table(y_hat),
    measure.vars=dvars_model$model_data$var_names
  )

  # Compute uncertainty:
  uncertainty <- uncertainty(dvars_model, X)
  uncertainty <- data.table::melt(
    data.table::data.table(uncertainty),
    measure.vars=dvars_model$model_data$var_names
  )

  # Add date if possible:
  if (is.null(X)) {
    predictions[,date:=dvars_model$model_data$data[,date][1:(.N)],by=variable]
  } else {
    predictions[,date:=1:(.N),by=variable]
  }
  data.table::setcolorder(predictions, "date")

  if (is.null(X)) {
    uncertainty[,date:=dvars_model$model_data$data[,date][1:(.N)],by=variable]
  } else {
    uncertainty[,date:=1:(.N),by=variable]
  }
  data.table::setcolorder(uncertainty, "date")

  # Return predictions:
  predictions <- list(
    predictions = predictions,
    uncertainty = uncertainty,
    X = X,
    model = dvars_model
  )
  class(predictions) <- "predictions"
  return(predictions)
}

## Loss: ----
#' @export
loss.dvars_model <- function(dvars_model, X=NULL, y=NULL) {
  res <- data.table::data.table(residuals(dvars_model, X=X, y=y))
  lags <- dvars_model$model_data$lags
  res[,date:=dvars_model$model_data$data[,date][(lags+1):(.N+lags)]]
  res <- data.table::melt(res, id.vars="date")
  return(res)
}

#' @export
loss <- function(dvars_model, X=NULL, y=NULL) {
  UseMethod("loss", dvars_model)
}

## Mean squared error (MSE): ----
#' @export
mse.dvars_model <- function(dvars_model, X=NULL, y=NULL) {

  res <- loss(dvars_model, X=X, y=y)
  mse <- res[,.(value=mean((value)^2)),by=variable]

  return(mse)
}

#' @export
mse <- function(dvars_model, X=NULL, y=NULL) {
  UseMethod("mse", dvars_model)
}

## Root mean squared error (RMSE): ----
#' @export
rmse.dvars_model <- function(dvars_model, X=NULL, y=NULL) {

  res <- loss(dvars_model, X=X, y=y)
  rmse <- res[,.(value=sqrt(mean((value)^2))),by=variable]

  return(rmse)
}

#' @export
rmse <- function(dvars_model, X=NULL, y=NULL) {
  UseMethod("rmse", dvars_model)
}

## Cumulative loss: ----
#' @export
cum_loss.dvars_model <- function(dvars_model, X=NULL, y=NULL) {

  res <- loss(dvars_model, X=X, y=y)
  cum_loss <- list(cum_loss = res[,.(date=date, value=cumsum(value^2)),by=variable])
  class(cum_loss) <- "cum_loss"
  return(cum_loss)
}

#' @export
cum_loss <- function(dvars_model, X=NULL, y=NULL) {
  UseMethod("cum_loss", dvars_model)
}

## Forecasting: ----
#' @export
forecast.dvars_model <- function(dvars_model, n.ahead=1) {

  # Set up:
  var_names <- dvars_model$model_data$var_names
  lags <- dvars_model$model_data$lags
  sample <- data.table::copy(dvars_model$model_data$data)
  if (!"date" %in% names(sample)) {
    sample[,date:=1:.N]
  }
  fcst <- data.table::copy(sample[.N,])
  uncty <- data.table::copy(sample[.N,])
  uncty[,(var_names):=0]
  data <- sample
  counter <- 1
  increment_date <- ifelse(
    sample[,class(date)=="Date"],
    round(sample[,mean(diff(date))]),
    1
  )

  # Forecast recursively:
  while(counter <= n.ahead) {
    X <- prepare_predictors(dvars_model, data[,.SD,.SDcols=var_names])
    predictive <- predict(dvars_model, X)
    y_hat <- predictive$predictions
    uncertainty <- predictive$uncertainty

    # Update
    fcst_t <- data.table::dcast(y_hat, .~variable)[,-1]
    fcst_t[,date:=data[.N,date+increment_date]]
    fcst <- rbind(fcst, fcst_t)

    uncty_t <- data.table::dcast(uncertainty, .~variable)[,-1]
    uncty_t[,date:=data[.N,date+increment_date]]
    uncty <- rbind(uncty, uncty_t)

    data <- rbind(data, fcst_t)
    counter <- counter + 1
  }
  data.table::setcolorder(fcst, "date")

  # Return:
  fcst <- list(
    fcst = fcst,
    uncty = uncty,
    model_data = dvars_model$model_data
  )
  class(fcst) <- "forecast"

  return(fcst)

}

#' @export
forecast <- function(dvars_model, n.ahead=1) {
  UseMethod("forecast", dvars_model)
}
