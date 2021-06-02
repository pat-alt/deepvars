# Mean orecasting error: ----
#' @export
msfe.forecast <- function(forecast, y_true) {
  msfe <- colMeans(forecast$fcst[-1,(y_test-.SD)^2,.SDcols=colnames(y_test)])
  msfe <- data.table::data.table(value=msfe, variable=names(msfe))
  return(msfe)
}

#' @export
msfe <- function(forecast, y_true) {
  UseMethod("msfe", forecast)
}

# Root mean forecasting error: ----
#' @export
rmsfe.forecast <- function(forecast, y_true) {
  rmsfe <- sqrt(colMeans(forecast$fcst[-1,(y_test-.SD)^2,.SDcols=colnames(y_test)]))
  rmsfe <- data.table::data.table(value=rmsfe, variable=names(rmsfe))
  return(rmsfe)
}

#' @export
rmsfe <- function(forecast, y_true) {
  UseMethod("rmsfe", forecast)
}

# Forecast correlations: ----
#' @export
cor_fcst.forecast <- function(forecast, y_true) {
  var_names <- colnames(y_test)
  cor_fcst <- sapply(
    var_names,
    function(var) {
      cor(forecast$fcst[-1,get(var)],y_test[,var])
    }
  )
  cor_fcst <- data.table::data.table(value=cor_fcst, variable=names(cor_fcst))
  return(cor_fcst)
}

#' @export
cor_fcst <- function(forecast, y_true) {
  UseMethod("cor_fcst", forecast)
}

# Print, plot, ... ----
#' @export
print.forecast <- function(forecast) {
  print(forecast$fcst)
}

#' @export
plot.forecast <- function(forecast, y_true=NULL, history=NULL) {

  # Forecasts
  K <- forecast$model_data$K
  sample <- data.table::copy(forecast$model_data$data)
  sample[,type:="Actual"]
  if (!"date" %in% names(sample)) {
    sample[,date:=1:.N]
  }
  fcst <- forecast$fcst[,type:="Forecast"]
  dt_plot <- rbind(sample,fcst)
  dt_plot <- data.table::melt(dt_plot, id.vars = c("date","type"))
  if (!is.null(history)) {
    increment_date <- ifelse(sample[,class(date)=="Date"], round(sample[,mean(diff(date))]), 1)
    dt_plot <- dt_plot[date >= sample[,max(date)]-history*increment_date]
  }

  # True outcomes:
  if (!is.null(y_true)) {
    y_true <- data.table::data.table(y_true)
    y_true[,date:=fcst$date[-1]]
    y_true[,type:="Actual"]
    # y_true <- rbind(sample[.N,],y_true)
    y_true <- data.table::melt(y_true, id.vars = c("date","type"))
    dt_plot <- rbind(dt_plot,y_true)
  }


  p <- ggplot2::ggplot(data=dt_plot) +
    ggplot2::geom_line(ggplot2::aes(x=date, y=value, linetype=type)) +
    ggplot2::facet_wrap(.~variable, nrow = K, scales = "free_y") +
    ggplot2::scale_linetype_discrete(name="Type:") +
    ggplot2::labs(
      x="Date",
      y="Value"
    )
  p

  return(p)
}
