#' irf
#'
#' @param varresult
#' @param imp
#' @param structural
#' @param n_ahead
#' @param size
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer

irf = function(varresult,
               imp,
               structural = T,
               n_ahead = 10,
               size = 1,
               bootstrap = T,
               n_bootstrap = 1000,
               parametric = F,
               ci = .95,
               plot = T,
               cum = T) {

  # Get outputs ----
  A = varresult$A # coefficient matrix
  K = varresult$K # number of variables in the system
  N = varresult$N
  var_names = varresult$var_names
  lag = varresult$lag
  Sigma_res = varresult$Sigma_res
  res = varresult$res
  Y = varresult$Y
  Z = varresult$Z
  constant = varresult$constant
  const = varresult$const
  data = varresult$data

  # Compute IRF ----
  compute_IRF = function(varresult,
                         imp,
                         structural,
                         n_ahead ,
                         size) {

    # Get outputs ----
    A = varresult$A # coefficient matrix
    K = varresult$K # number of variables in the system
    N = varresult$N
    var_names = varresult$var_names
    lag = varresult$lag
    Sigma_res = varresult$Sigma_res
    res = varresult$res
    Y = varresult$Y
    Z = varresult$Z
    constant = varresult$constant
    const = varresult$const
    data = varresult$data

    # Pre-processing ----
    imp = ifelse(class(imp) == "character", which(var_names == imp), imp)
    # Choleski decomposition - lower triangular:
    B_0 = identify_chol(Sigma_res)

    # Initialization ----
    # Shocks:
    u = rep(0,K)
    names(u) = var_names
    u[imp] = size * sqrt(diag(Sigma_res))[imp] # one standard deviation shock
    # Structural shocks?:
    if (structural) {
      pre_multiply_shocks = B_0
    } else {
      pre_multiply_shocks = diag(K)
    }
    shocks = pre_multiply_shocks %*% u
    # Vector of regressors:
    X = matrix(rep(0,K * lag), nrow=K * lag)
    names(X) = colnames(Z)[(1+const):ncol(Z)]
    # Coefficients:
    A_trans = t(A[(1+const):nrow(A),])
    # Container for state responses:
    irf = matrix(NA, nrow = n_ahead+1, ncol=K)
    colnames(irf) = var_names
    # Initial shocks at time zero:
    Y_h = A_trans %*% X + shocks
    irf[1,] = Y_h
    # Update vector of regressors (lagged values of Y):
    X = c(Y_h,X)[1:(K * lag)]
    names(X) = colnames(Z)[(1+const):ncol(Z)]

    # Recursion ----
    for (h in 2:(n_ahead+1)) {

      Y_h = A_trans %*% X
      # Update vector of regressors:
      X = c(Y_h,X)[1:(K * lag)]
      names(X) = colnames(Z)[(1+const):ncol(Z)]
      # Update IRF
      irf[h,] = Y_h

    }

    # Tidy up ----
    irf = data.table::melt(data.table::as.data.table(irf), measure.vars = colnames(irf), value.name = "irf")
    irf[,cum_irf:=cumsum(irf),by=variable]
    irf[,h:=1:.N,by=variable]
    irf = data.table::melt(irf, id.vars = c("variable", "h"), variable.name = "type")

    return(irf)

  }

  irf_output = compute_IRF(varresult,
                           imp,
                           structural,
                           n_ahead,
                           size)

  # Bootstrapped confidence intervals ----
  if (bootstrap==T) {

    bs = lapply(1:n_bootstrap, function(x) {

      # STEP 1 - sample from residual matrix with replacement ----
      if(parametric) {
        # Parametric bootstrap:
        res_star = sapply(1:ncol(res), function(i) {
          stats::rnorm(nrow(res), mean = mean(res[,i]), sd=stats::sd(res[,i]))
        })
      } else {
        # Non-parametric
        res_star = res[sample(nrow(res),nrow(res), replace = T),]
      }

      # STEP 2 - simulate VAR(p) recursively
      # Initialization
      Y_star = Y[1:lag,] # pre-sample values actually realized

      # Recursion
      for (i in 1:nrow(res_star)) {

        # Regressor
        if (constant==T) {
          X = c(1,t(Y_star[nrow(Y_star):(nrow(Y_star)-lag+1),]))
        } else {
          X = c(t(Y_star[nrow(Y_star):(nrow(Y_star)-lag+1),]))
        }

        Y_hat = X %*% A + res_star[i,] # 1-step ahead prediction
        # Update y_star:
        Y_star = rbind(Y_star, Y_hat)

      }

      # STEP 3 - estimate VAR(p) for simulated data ----
      varresult_star = VAR(Y_star,lag, constant = constant, verbose = F)

      # STEP 4 - estimate IRF using new coefficients ----
      irf_star = compute_IRF(varresult = varresult_star,
                             imp = imp,
                             n_ahead = n_ahead,
                             structural = structural,
                             size = size)

      # Prepare ----
      irf_star = data.table::as.data.table(irf_star)
      # irf_star[,h:=1:.N,by=variable]
      irf_star[,n_bs:=x]

      return(irf_star)

    })
    bs = data.table::rbindlist(bs)

    # STEP 5 - compute bands ----
    p = (1 - ci)/2
    ci_bands = bs[,.(upper=stats::quantile(value, probs = 1-p),
                     lower=stats::quantile(value, probs = p)),
                  by = .(variable, h, type)]

    irf_output = merge(irf_output, ci_bands, on=c("variable", "h", "type"))

  }

  # Plot ----
  if (plot == T) {
    if(cum==T) {
      chart_data = irf_output[type=="cum_irf"]
    } else {
      chart_data = irf_output[type=="irf"]
    }

    # Chart:
    p = ggplot2::ggplot(data=chart_data) +
      ggplot2::geom_line(ggplot2::aes(x=h, y=value)) +
      ggplot2::scale_linetype(guide="none") +
      ggplot2::facet_wrap(.~variable, scales="free") +
      ggplot2::geom_line(ggplot2::aes(x=h, y=upper, linetype="dotted"),
                colour="red") +
      ggplot2::geom_line(ggplot2::aes(x=h, y=lower, linetype="dotted"),
                colour="red")

    print(p)
  }

  return(list("irf" = irf,
              "n_ahead" = n_ahead,
              "varresult" = varresult,
              "imp" = imp,
              "cum" = cum,
              "structural" = structural,
              "size" = size))

}
