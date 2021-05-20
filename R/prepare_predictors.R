#' Title
#'
#' @param var_data
#'
#' @description Take a `var_data` object and prepares predictors for recursive forecasts.
#'
#' @return
#' @export
#'
#' @examples
prepare_predictors <- function(data, lags) {

  # Explanatory variables:
  X = as.matrix(
    data[
      (.N-(lags-1)):.N, # take last p rows
      sapply(
        0:(lags-1),
        function(lag) {
          data.table::shift(.SD, lag)
        }
      )
    ][.N,] # take last row of that
  )

  return(X)

}
