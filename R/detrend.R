#' detrend
#'
#' @param y
#'
#' @return
#' @export
detrend = function(y) {
  y = as.matrix(y)
  sapply(1:ncol(y), function(i) {
    y_i = y[,i]
    t = 1:length(y_i)
    stats::lm(y_i ~ t)$res
  })
}
