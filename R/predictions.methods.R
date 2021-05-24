# Plotting
#' @export
print.predictions <- function(predictions) {
  print(predictions$predictions)
}

#' @export
plot.predictions <- function(predictions, y_true=NULL) {
  pred <- predictions$predictions
  pred[,type:="Prediction"]
  model_data <- predictions$model$model_data
  if (!is.null(y_true)) {
    y_true <- data.table::data.table(y_true)
    y_true[,type:="Actual"]
    y_true <- data.table::melt(y_true, id.vars = "type")
    y_true[,date:=pred$date]
  }
  dt_plot <- rbind(pred,y_true)
  p <- ggplot2::ggplot(data=dt_plot, ggplot2::aes(x=date, y=value, colour=type)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_discrete(name="Type:") +
    ggplot2::facet_wrap(
      ~variable,
      scales="free_y",
      nrow = dt_plot[,length(unique(variable))]
    ) +
    ggplot2::labs(
      x="Date",
      y="Value"
    )
  p
  return(p)
}
