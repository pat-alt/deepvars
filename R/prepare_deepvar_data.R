#'
#' elec_dataset <- dataset(
#'   name = "elec_dataset",
#'
#'   initialize = function(x, lags, sample_frac = 1) {
#'
#'     self$lags <- lags
#'     self$x <- torch_tensor((x - train_mean) / train_sd)
#'
#'     n <- length(self$x) - self$lags
#'
#'     self$starts <- sort(sample.int(
#'       n = n,
#'       size = n * sample_frac
#'     ))
#'
#'   },
#'
#'   .getitem = function(i) {
#'
#'     start <- self$starts[i]
#'     end <- start + self$lags - 1
#'
#'     list(
#'       x = self$x[start:end],
#'       y = self$x[end + 1]
#'     )
#'
#'   },
#'
#'   .length = function() {
#'     length(self$starts)
#'   }
#' )
#'
#'
#' #' @export
#' prepare_deepvar_data <- function(data, lags=1, horizon=1, type="var") {
#'
#'   if (type=="var") {
#'     var_data <- prepare_var_data(data, lags = lags, standardize = TRUE)
#'     deepvar_data <- var2deepvar(var_data)
#'   } else if (type=="standard") {
#'     deepvar_data <- deepvar_standard(data, lags=lags, horizon=1)
#'   }
#'
#'   return(deepvar_data)
#'
#' }
