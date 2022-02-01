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
