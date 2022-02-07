# Root mean prediction error: ----
#' @export
rmsfe.prediction <- function(prediction, y) {
  y_hat <- prediction$prediction
  rmsfe <- sqrt(colMeans((y-y_hat)**2))
  return(rmsfe)
}

#' @export
rmsfe <- function(prediction, y) {
  UseMethod("rmsfe", prediction)
}

# Prediction correlations: ----
#' @export
cor_prediction.prediction <- function(prediction, y) {
  y_hat <- prediction$prediction
  cor_prediction <- sapply(1:ncol(y_hat), function(k) cor(y_hat[,k],y[,k]))
  return(cor_prediction)
}

#' @export
cor_prediction <- function(prediction, y) {
  UseMethod("cor_prediction", prediction)
}

# Print, plot, ... ----
#' @export
print.prediction <- function(prediction) {
  print(prediction$prediction)
}

#' @export
plot.prediction <- function(
  prediction,
  y=NULL,
  history=NULL,
  plot_ci=FALSE,
  ci=.95,
  ci_colour="darkblue"
) {

  dates <- NULL # might be changed to argument in future

  # Predictions
  K <- prediction$model_data$K

  if (is.null(prediction$input_ds)) {
    # In case predictions are from end of training sample (no input_ds supplied)
    sample <- data.table::data.table(prediction$model_data$data)
  } else {
    # In case input_ds supplied to prediction()
    sample <- data.table::data.table(prediction$input_ds)
  }

  prediction <- rbind(sample[.N,],data.table::data.table(prediction$prediction))[,type:="Prediction"]
  uncty <- rbind(sample[.N,],data.table::data.table(prediction$uncty))[,type:="Prediction"]
  sample[,type:="Actual"]
  if (is.null(dates)) {
    sample[,date:=1:.N]
    prediction[,date:=(nrow(sample)):(nrow(sample)+.N-1)]
    uncty[,date:=(nrow(sample)):(nrow(sample)+.N-1)]
  }

  dt_plot <- rbind(sample,prediction)
  dt_plot <- data.table::melt(dt_plot, id.vars = c("date","type"))

  # True outcomes:
  if (!is.null(y)) {
    y <- data.table::data.table(y)[1:(nrow(prediction)-1)]
    y[,date:=prediction$date[-1]]
    y[,type:="Actual"]
    y <- data.table::melt(y, id.vars = c("date","type"))
    dt_plot <- rbind(dt_plot,y,fill=TRUE)
  }

  # Uncertainty:
  if (plot_ci) {
    uncty <- data.table::melt(uncty, id.vars = c("date","type"), value.name = "uncertainty")
    dt_plot <- merge(dt_plot, uncty, on="date", all = TRUE)
    dt_plot[is.na(uncertainty), uncertainty:=0]
    p = (1 - ci)/2
    q = abs(stats::qnorm(p))
  }

  # History:
  if (!is.null(history)) {
    dt_plot <- dt_plot[date >= sample[,max(date)]-history]
  }

  if (plot_ci) {
    p <- ggplot2::ggplot(data=dt_plot, ggplot2::aes(x=date, y=value)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin=value-q*uncertainty, ymax=value+q*uncertainty, group=type),
        alpha=0.3,
        fill=ci_colour,
        colour=ci_colour,
        size=0.25
      )
  } else {
    p <- ggplot2::ggplot(data=dt_plot, ggplot2::aes(x=date, y=value))
  }

  # PLot:
  p <- p +
    ggplot2::geom_line(ggplot2::aes(linetype=type)) +
    ggplot2::facet_wrap(.~variable, nrow = K, scales = "free_y") +
    ggplot2::scale_linetype_discrete(name="Type:") +
    ggplot2::labs(
      x="Date",
      y="Value"
    )
  p

  return(p)
}
