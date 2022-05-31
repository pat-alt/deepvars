#' lag_order
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
lag_order <- function(data, max_lag=10, ic_choice="AIC", type="const") {

  # Preparing same sample for each fit:
  var_data <- prepare_var_data(data, lags = max_lag)
  N <- var_data$N
  y <- var_data$y
  X <- var_data$X
  K <- var_data$K
  constant <- var_data$constant
  lag_idx <- seq(K, K * max_lag, K) + constant

  # Recursion: ----
  output <- lapply(
    1:max_lag,
    function(m) {

      # Fit model:
      X_temp <- X[,1:lag_idx[m]] # use only past m lags
      res <- lm.fit(x=X_temp, y=y)$residuals
      Sigma_res <- crossprod(res)/N

      # Compute criteria:
      AIC = log(det(Sigma_res)) + (2/N) * (K^2 * m + K) # Akaike
      HQC = log(det(Sigma_res)) + ((2 * log(log(N))) / N) * (K^2 * m + K) # Hannan-Quinn
      SIC = log(det(Sigma_res)) + (log(N)/(N)) * (K^2 * m + K) # Schwarze/Bayes
      criteria = data.table::data.table(
        m = m,
        SIC = SIC,
        HQC = HQC,
        AIC = AIC
      )

    }
  )

  # Output: ----
  criteria_overview = data.table::rbindlist(output)
  proposed_lag_lengths = data.table::melt(criteria_overview, id.vars="m", variable.name = "ic")
  proposed_lag_lengths[,min:=min(value, na.rm=T), by="ic"]
  proposed_lag_lengths[,lag:=which.min(value), by="ic"]
  proposed_lag_lengths = proposed_lag_lengths[,unique(.SD),.SDcols = c("ic", "min", "lag")]
  p = proposed_lag_lengths[ic==ic_choice, lag]

  return(
    list(
      criteria_overview = criteria_overview,
      proposed_lag_lengths = proposed_lag_lengths,
      p = p
    )
  )

}

#' companion_form
#'
#' @param A
#' @param constant
#'
#' @return
#' @export
#'
#' @examples
companion_form <- function(A, constant=TRUE) {
  K <- ncol(A)
  lags <- (nrow(A)-constant)/K
  if(constant==TRUE) {
    top <- t(A[-1,])
  } else {
    top <- t(A)
  }
  bottom <- cbind(
    diag(K*(lags-1)),
    matrix(rep(0,K**2 * (lags-1)),ncol = K)
  )
  A_comp <- rbind(top,bottom)
  return(A_comp)
}

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

#' compute_J
#'
#' @param K
#' @param lag
#'
#' @return
#'
#' @details Helper function to create the allocation matrix J, which can be used to transform a $VAR(p)$ into $VMA$ represntation.
#'
compute_J = function(K, lags) {

  J = matrix(0,nrow=K,ncol=K*lags)
  diag(J) = 1
  return(J)

}

#' compute_Phi
#'
#' @param lag
#' @param K
#' @param A_comp
#' @param const
#' @param n.ahead
#'
#' @importFrom expm `%^%`
#'
#' @return
#'
#' @description Computes the Phi coefficients (reduced form) in Kilian, Luetkepohl.
#'
compute_Phi = function(lags, K, A_comp, const, n.ahead) {

  J = compute_J(K, lags)

  Phi = lapply(1:n.ahead, function(i) {
    J %*% (A_comp %^% (i-1)) %*% t(J)
  })

  return(Phi)

}
