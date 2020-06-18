#' detrend
#'
#' @param Y
#'
#' @return
#' @export
detrend = function(Y) {
  Y = as.matrix(Y)
  sapply(1:ncol(Y), function(i) {
    y = Y[,i]
    t = 1:length(y)
    stats::lm(y ~ t)$res
  })
}
