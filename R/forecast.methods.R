#' @export
print.forecast <- function(forecast) {
  print(forecast$fcst)
}

#' @export
plot.forecast <- function(forecast, history=NULL) {

  K <- forecast$model_data$K
  sample <- copy(forecast$model_data$data)
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

  p <- ggplot2::ggplot(data=dt_plot) +
    ggplot2::geom_line(ggplot2::aes(x=date, y=value, linetype=type)) +
    facet_wrap(.~variable, nrow = K, scales = "free_y") +
    scale_linetype_discrete(name="Type:") +
    labs(
      x="Date",
      y="Value"
    )
  p

  return(p)
}
