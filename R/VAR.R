#' VAR
#'
#' @param data Data set
#' @param lag Number of lags
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer

VAR = function(data, lag, method="ls", constant=T, ci=.95, verbose=T) {

  # Preprocessing ----
  var_names = colnames(data)
  N = nrow(data)-lag
  K = ncol(data)
  var_names = colnames(data)
  data = data.table::as.data.table(data)
  data_out = data.table::as.data.table(data)
  # Data and lags:
  data_out[,(c(sapply(1:lag, function(p) sprintf("%s_l%i", var_names, p)))) := sapply(1:lag, function(p) data.table::shift(.SD, p))]
  # Dependent variable:
  Y = as.matrix(data_out[(lag+1):.N,1:K])
  # Explanatory variables:
  if (constant==T) {
    const = 1
    Z = cbind("constant"=1,as.matrix(data_out[(lag+1):.N,(K+1):ncol(data_out)]))
  } else {
    const = 0
    Z = as.matrix(data_out[(lag+1):.N,(K+1):ncol(data_out)])
  }
  df = (N-K*lag-const) # degrees of freedom

  # Least-squares estimation ----
  if (method=="ls") {
    # Summary statistics:
    A = solve(crossprod(Z), crossprod(Z,Y)) # coefficients
    Y_hat = Z %*% A
    res = Y - Y_hat # residuals
    Sigma_res = crossprod(res)/(N-K*lag-const) # vcov of residuals
    Sigma_A = kronecker(chol2inv(chol(crossprod(Z))), Sigma_res) # vcov of coefficients
    se = matrix(sqrt(diag(Sigma_A)), nrow = (const + lag * K), byrow = T) # standard errors
    colnames(se) = colnames(A)
    rownames(se) = rownames(A)
    t_val = A / se # t values
    p_val = stats::pt(-abs(t_val), df) * 2 # p values, i.e. Prob(> |t|)
    # Confidence interval:
    p = (1 - ci)/2
    q = abs(stats::qt(p, df))
    upper = A + q * se
    lower = A - q * se
  }

  # Maximum likelihood estimation ----
  if (method=="mle") {
    nll = function (A) {

      A = matrix(A, nrow=(const + lag * K))
      # Params:
      res = Y - Z %*% A
      Sigma_res = crossprod(res)/(N-1)
      # NLL:
      nll = ( (K * N) / 2 * log(2*pi) ) +
        ( N/2 * log(det(Sigma_res)) ) +
        sum(sapply(1:N, function(i) t(res[i,]) %*% solve(Sigma_res) %*% res[i,]))

      return(nll)

    }
    A_0 = data.table::melt(data.table::as.data.table(solve(crossprod(Z), crossprod(Z,Y))))$value # initial guess
    output = stats::optim(par=A_0, fn=nll, method="BFGS")
    # Coefficients:
    A = matrix(output$par, nrow=(const + lag * K))
    rownames(A) = colnames(Z)
    colnames(A) = colnames(Y)
    # Covariance matrix:
    Y_hat = Z %*% A
    res = Y - Y_hat # residuals
    Sigma_res = crossprod(res)/(N-K*lag-const) # vcov of residuals
    Sigma_A = kronecker(chol2inv(chol(crossprod(Z))), Sigma_res) # vcov of coefficients
    se = matrix(sqrt(diag(Sigma_A)), nrow = (const + lag * K), byrow = T) # standard errors
    colnames(se) = colnames(A)
    rownames(se) = rownames(A)
    t_val = A / se # t values
    p_val = stats::pt(-abs(t_val), df) * 2 # p values, i.e. Prob(> |t|)
    # Confidence interval:
    p = (1 - ci)/2
    q = abs(stats::qt(p, df))
    upper = A + q * se
    lower = A - q * se

  }

  # Tidy summary ----
  from = rownames(A)
  coeff_tidy = data.table::as.data.table(A)
  coeff_tidy[,from:=from]
  coeff_tidy = data.table::melt(coeff_tidy,
                    id.vars = "from",
                    value.name = "estimate",
                    variable.name = "to")
  coeff_tidy[,se:=c(se)]
  coeff_tidy[,t_value:=c(t_val)]
  coeff_tidy[,p_value:=c(p_val)]
  coeff_tidy[,signif:=ifelse(p_value>0.1,"",
                             ifelse(p_value <= 0.1 & p_value > 0.05, ".",
                                    ifelse(p_value <= 0.05 & p_value > 0.01, "*",
                                           ifelse(p_value <= 0.01 & p_value > 0.001,"**","***"))))]
  coeff_tidy[,upper:=c(upper)]
  coeff_tidy[,lower:=c(lower)]
  print_cols = c("from",
                 "to",
                 "estimate",
                 "se",
                 "t_value",
                 "p_value",
                 "signif")
  if (verbose ==T) {
    print(coeff_tidy[,.SD,.SDcols=print_cols])
  }

  # Companion-form matrix ----
  if(constant==T) {
    top = t(A[-1,])
  } else {
    top = t(A)
  }
  bottom = cbind(diag(K*(lag-1)),
                 matrix(rep(0,K**2 * (lag-1)),ncol = K))
  A_comp = rbind(top,bottom)

  # Stacked:
  # stacked = data.table::melt(A)
  # b = stacked$value
  # names(b) = sprintf("%s_%s", stacked[,1], stacked[,2])

  # Restricted least-squares:
  # if (!is.null(restrict)) {
  #
  #   M = (K^2 * lag + K) - length(restrict) * lag
  #   R = matrix(rep(0, (K^2 * lag + K) * M), (K^2 * lag + K), M)
  #
  #   R = diag( K^2 * lag + K )
  #   sapply(1:length(restrict), function (r) {
  #
  #     res_temp = restrict[[r]]
  #
  #     R_new = sapply(1:lag, function(p) {
  #
  #       idx = (1 * res_temp[1]) + (p * res_temp[2])
  #       R[idx,idx] = 0
  #       R = R[,-idx]
  #
  #       return(R)
  #
  #     })
  #
  #   })
  #
  #   Q = kronecker(crossprod(Z), information)
  #   A = crossprod()
  #
  # }

  return(
    list(
      coeff_tidy = coeff_tidy,
      A  = A,
      constant = constant,
      const = const,
      A_comp = A_comp,
      res = res,
      Sigma_res = Sigma_res,
      Sigma_A = Sigma_A,
      se = se,
      df = df,
      t_val = t_val,
      p_val = p_val,
      upper = upper,
      lower = lower,
      data_out = data_out,
      data = data,
      Y = Y,
      Z = Z,
      Y_hat = Y_hat,
      lag = lag,
      K = K,
      N = N,
      var_names = var_names
    )
  )

}
