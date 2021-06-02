#' VAR_lag_select
#'
#' @param data
#' @param max_lag
#' @param ic_choice
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @return
#' @export
#'
VAR_lag_select = function(data, max_lag=10, ic_choice="AIC") {

  # Run loop ----
  output <- lapply(
    1:max_lag,
    function(m) {
      var_model <- vareg(data, lags=m)
      N <- var_model$model_data$N
      K <- var_model$model_data$K
      res <- var_model$res
      Sigma_res = crossprod(res)/N
      AIC = log(det(Sigma_res)) + (2/N) * ((K ** 2) * m + K) # Akaike
      HQC = log(det(Sigma_res)) + ((2 * log(log(N))) / N) * ((K ** 2) * m + K) # Hannan-Quinn
      SIC = log(det(Sigma_res)) + (log(N)/(N)) * ((K ** 2) * m + K) # Schwarze/Bayes
      criteria = data.table::data.table(
        m = m,
        SIC = SIC,
        HQC = HQC,
        AIC = AIC
      )

    }
  )

  # Bundle output ----
  criteria_overview = data.table::rbindlist(output)
  proposed_lag_lengths = data.table::melt(criteria_overview, id.vars="m", variable.name = "ic")
  proposed_lag_lengths[,min:=min(value, na.rm=T), by="ic"]
  proposed_lag_lengths[,lag:=which.min(value), by="ic"]
  proposed_lag_lengths = proposed_lag_lengths[,unique(.SD),.SDcols = c("ic", "min", "lag")]

  # Lag given choice of information criterium ----
  p = proposed_lag_lengths[ic==ic_choice, lag]

  return(
    list(
      criteria_overview = criteria_overview,
      proposed_lag_lengths = proposed_lag_lengths,
      p = p
    )
  )

}
