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
  output = lapply(1:max_lag, function(i) {

    lag = i

    # Preprocessing ----
    N = nrow(data)-lag
    K = ncol(data)
    var_names = colnames(data)
    data_out = data.table::as.data.table(data)
    # Data and lags:
    data_out[,(c(sapply(1:lag, function(p) sprintf("%s_l%i", var_names, p)))) := sapply(1:lag, function(p) data.table::shift(.SD, p))]
    # Dependent variable:
    Y = as.matrix(data_out[(lag+1):.N,1:K])
    # Explanatory variables:
    Z = cbind("constant"=1,as.matrix(data_out[(lag+1):.N,(K+1):ncol(data_out)]))

    # Least-squares estimation ----
    A = try(solve(crossprod(Z), crossprod(Z,Y)))

    if (class(A)!="try-error") {
      res = Y - Z %*% A
      Sigma_res = crossprod(res)/(N-1)

      # Information criteria ----
      AIC = log(det(Sigma_res)) + (2/(N-lag)) * ((K ** 2) * lag + K) # Akaike
      HQC = log(det(Sigma_res)) + (2 * log(log(N-lag)) /(N-lag)) * ((K ** 2) * lag + K) # Hannan-Quinn
      BIC = log(det(Sigma_res)) + (log(N-lag)/(N-lag)) * ((K ** 2) * lag + K) # Schwarze/Bayes
      criteria = data.table::data.table(m = lag,
                            BIC = BIC,
                            HQC = HQC,
                            AIC = AIC)
    } else {
      criteria = data.table::data.table(m=lag,
                            BIC=NA,
                            HQC=NA,
                            AIC=NA)
    }


  })

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
