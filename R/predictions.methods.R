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
    y_true <- melt(y_true, id.vars = "type")
  }
  dt_plot <- rbind(pred,y_true)
  dt_plot[,date:=1:(.N),by=.(variable, type)]
  p <- ggplot(data=dt_plot, aes(x=date, y=value, colour=type)) +
    geom_line() +
    scale_color_discrete(name="Type:") +
    facet_wrap(
      ~variable,
      scales="free_y",
      nrow = dt_plot[,length(unique(variable))]
    ) +
    labs(
      x="T",
      y="Value"
    )
  p
  return(p)
}
