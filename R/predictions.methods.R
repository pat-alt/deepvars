# Plotting
#' @export
print.predictions <- function(predictions) {
  print(predictions$predictions)
}

#' @export
plot.predictions <- function(
  predictions,
  y_true=NULL,
  plot_ci=FALSE,
  ci=.95,
  alpha_ci=0.1
) {

  # Fitted values:
  pred <- predictions$predictions
  pred[,type:="Prediction"]
  model_data <- predictions$model$model_data

  # True outcomes:
  if (!is.null(y_true)) {
    y_true <- data.table::data.table(y_true)
    y_true[,type:="Actual"]
    y_true <- data.table::melt(y_true, id.vars = "type")
    y_true[,date:=pred$date]
  }
  dt_plot <- rbind(pred,y_true)

  # Uncertainty:
  if (plot_ci) {
    uncty <- predictions$uncertainty[,type:="Prediction"]
    setnames(uncty, "value", "uncertainty")
    dt_plot <- merge(dt_plot, uncty, on="date", all = TRUE)
    dt_plot[is.na(uncertainty), uncertainty:=0]
    p = (1 - ci)/2
    q = abs(stats::qnorm(p))
    p <- ggplot2::ggplot(data=dt_plot, ggplot2::aes(x=date, y=value, colour=type)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin=value-q*uncertainty, ymax=value+q*uncertainty, group=type, fill=type),
        linetype="dashed",
        alpha=alpha_ci,
        size=0.25
      )
  } else {
    p <- ggplot2::ggplot(data=dt_plot, ggplot2::aes(x=date, y=value, colour=type))
  }


  p <- p +
    ggplot2::geom_line() +
    ggplot2::scale_color_discrete(name="Type:") +
    ggplot2::scale_fill_discrete(name="Type:") +
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
