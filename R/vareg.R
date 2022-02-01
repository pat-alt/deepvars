#' vareg
#'
#' @param data Data set
#' @param lags Number of lags
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer
vareg <- function(data, lags=1, method="ols", constant=TRUE, ci=.95, standardize=FALSE) {

  var_data <- prepare_var_data(data, lags=lags, constant=constant)
  list2env(var_data, envir = environment()) # unpack VAR data
  df <- (N-K*lags-constant) # degrees of freedom

  # Least-squares estimation ----
  if (method=="ols") {
    # Summary statistics:
    A = solve(crossprod(X), crossprod(X,y)) # coefficients
    y_hat = X %*% A
    res = y - y_hat # residuals
    Sigma_res = crossprod(res)/df # vcov of residuals
    Sigma_A = kronecker(chol2inv(chol(crossprod(X))), Sigma_res) # vcov of coefficients
    se = matrix(sqrt(diag(Sigma_A)), nrow = (constant + lags * K), byrow = T) # standard errors
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

      A = matrix(A, nrow=(constant + lags * K))
      # Params:
      res = y - X %*% A
      Sigma_res = crossprod(res)/(N-1)
      # NLL:
      nll = ( (K * N) / 2 * log(2*pi) ) +
        ( N/2 * log(det(Sigma_res)) ) +
        sum(sapply(1:N, function(i) t(res[i,]) %*% solve(Sigma_res) %*% res[i,]))

      return(nll)

    }
    A_0 = data.table::melt(data.table::as.data.table(solve(crossprod(X), crossprod(X,y))))$value # initial guess
    output = stats::optim(par=A_0, fn=nll, method="BFGS")
    # Coefficients:
    A = matrix(output$par, nrow=(constant + lags * K))
    rownames(A) = colnames(X)
    colnames(A) = colnames(y)
    # Covariance matrix:
    y_hat = X %*% A
    res = y - y_hat # residuals
    Sigma_res = crossprod(res)/(N-K*lags-constant) # vcov of residuals
    Sigma_A = kronecker(chol2inv(chol(crossprod(X))), Sigma_res) # vcov of coefficients
    se = matrix(sqrt(diag(Sigma_A)), nrow = (constant + lags * K), byrow = T) # standard errors
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
  coeff_tidy = data.table::melt(
    coeff_tidy,
    id.vars = "from",
    value.name = "estimate",
    variable.name = "to"
  )
  coeff_tidy[,se:=c(se)]
  coeff_tidy[,t_value:=c(t_val)]
  coeff_tidy[,p_value:=c(p_val)]
  coeff_tidy[
    ,
    signif:=ifelse(p_value>0.1,"",
                   ifelse(p_value <= 0.1 & p_value > 0.05, ".",
                          ifelse(p_value <= 0.05 & p_value > 0.01, "*",
                                 ifelse(p_value <= 0.01 & p_value > 0.001,"**","***"))))
  ]
  coeff_tidy[,upper:=c(upper)]
  coeff_tidy[,lower:=c(lower)]
  print_cols = c(
    "from",
    "to",
    "estimate",
    "se",
    "t_value",
    "p_value",
    "signif"
  )

  # Companion-form matrix ----
  A_comp <- companion_form(A, constant = constant)

  # Output ----
  var_model <- list(
    A  = A,
    A_comp = A_comp,
    constant = constant,
    res = res,
    Sigma_res = Sigma_res,
    Sigma_A = Sigma_A,
    se = se,
    df = df,
    t_val = t_val,
    p_val = p_val,
    upper = upper,
    lower = lower,
    X_train = X,
    y_train = y,
    y_hat = y_hat,
    coeff_tidy = coeff_tidy,
    model_data = var_data
  )
  class(var_model) <- c("var_model", "model")

  return(var_model)
}
