## Cumulative loss: ----
#' @export
cum_loss.model <- function(model) {
  cum_loss <- sapply(1:ncol(model$res), function(k) cumsum(model$res[,k]**2))
  class(cum_loss) <- "cum_loss"
  return(cum_loss)
}

#' @export
cum_loss <- function(model) {
  UseMethod("cum_loss", model)
}

#' @export
plot.cum_loss <- function(cum_loss, date=NULL) {
  cum_loss <- data.table::data.table(cum_loss)
  if (is.null(date)) {
    cum_loss[,date:=1:.N] |> data.table::melt(id.vars=c("date"))
  }
  p <- ggplot2::ggplot(data = cum_loss, ggplot2::aes(x=date, y=value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(.~variable, scales = "free_y") +
    ggplot2::labs(
      x="Date",
      y="Squared error"
    )
  p
  return(p)
}

