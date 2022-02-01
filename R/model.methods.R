## Loss: ----
#' @export
loss.model <- function(model, X=NULL, y=NULL) {
  res <- data.table::data.table(residuals(model, X=X, y=y))
  lags <- model$model_data$lags
  res[,date:=model$model_data$data[,date][(lags+1):(.N+lags)]]
  res <- data.table::melt(res, id.vars="date")
  return(res)
}

#' @export
loss <- function(model, X=NULL, y=NULL) {
  UseMethod("loss", model)
}

## Mean squared error (MSE): ----
#' @export
mse.model <- function(model, X=NULL, y=NULL) {

  res <- loss(model, X=X, y=y)
  mse <- res[,.(value=mean((value)^2)),by=variable]

  return(mse)
}

#' @export
mse <- function(model, X=NULL, y=NULL) {
  UseMethod("mse", model)
}

## Root mean squared error (RMSE): ----
#' @export
rmse.model <- function(model, X=NULL, y=NULL) {

  res <- loss(model, X=X, y=y)
  rmse <- res[,.(value=sqrt(mean((value)^2))),by=variable]

  return(rmse)
}

#' @export
rmse <- function(model, X=NULL, y=NULL) {
  UseMethod("rmse", model)
}

## Cumulative loss: ----
#' @export
cum_loss.model <- function(model, X=NULL, y=NULL) {

  res <- loss(model, X=X, y=y)
  cum_loss <- list(cum_loss = res[,.(date=date, value=cumsum(value^2)),by=variable])
  class(cum_loss) <- "cum_loss"
  return(cum_loss)
}

#' @export
cum_loss <- function(model, X=NULL, y=NULL) {
  UseMethod("cum_loss", model)
}
