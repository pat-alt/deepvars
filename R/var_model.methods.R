#' @export
print.var_model <- function(var_model) {
  print(knitr::kable(var_model$coeff_tidy))
}

## Predictions: ----
#' Fitted values
#'
#' @param var_model
#' @param X
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer
fitted.var_model <- function(var_model, X=NULL) {
  if (is.null(X)) {
    y_hat <- var_model$y_hat
  } else {
    y_hat <- tryCatch(
      X %*% var_model$A,
      error = function(e) {
        return(cbind(1,X) %*% var_model$A)
      }
    )
  }
  return(y_hat)
}

#' @export
uncertainty.var_model <- function(var_model, X=NULL, assume_gauss=TRUE) {
  if (assume_gauss) {
    uncertainty <- matrix(
      rep(sqrt(diag(var_model$Sigma_res)),var_model$model_data$N),
      ncol=var_model$model_data$K,
      byrow = TRUE
    )
    colnames(uncertainty) <- var_model$model_data$var_names
  } else {
    # PLACEHOLDER:
    warning("Not implemeneted.")
  }
  return(uncertainty)
}

#' @export
uncertainty <- function(var_model, X=NULL, assume_gauss=TRUE) {
  UseMethod("uncertainty", var_model)
}


#' @export
residuals.var_model <- function(var_model, X=NULL, y=NULL) {

  new_data <- new_data_supplied(X=X,y=y)

  if (new_data | is.null(var_model$res)) {
    y_hat <- fitted(var_model, X)
    res <- y - y_hat
  } else {
    res <- var_model$res
  }

  return(res)

}

#' @export
prepare_predictors.var_model <- function(var_model, data) {

  lags <- var_model$model_data$lags

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

#' @export
prepare_predictors <- function(var_model, data) {
  UseMethod("prepare_predictors", var_model)
}

# Other: ----

#' stability_var
#'
#' @param var_model Output from VAR estimation
#' @param verbose Logical: if TRUE, test result will be printed to console.
#'
#' @return
#' @export
#'
#'
#' @author Patrick Altmeyer
stability.var_model = function(var_model, verbose=T) {

  # Get outputs
  A_comp = var_model$A_comp # coefficient matrix in companion form
  K = var_model$K # number of variables in the system
  lag = var_model$lag

  # Perform test
  eigen_decomp = eigen(A_comp)
  eigenvals = eigen_decomp$values
  test_result = ifelse(max(abs(eigenvals)) < 1,"The VAR is stable.","The VAR is not stable")
  if (verbose==T) {
    message(test_result)
  }
  test_summary = data.table::data.table(lambda=eigenvals,in_unit_circle=T)
  test_summary[,in_unit_circle:=abs(eigenvals)<1]

  return(
    list(
      eigenvals = eigenvals,
      test_result = test_result
      # test_summary = test_summary
    )
  )

}

#' @export
stability <- function(var_model, verbose=T) {
  UseMethod("stability", var_model)
}

# Deprecated: ----
# NOTE: The following functions are deprecated and therefore not exported.

#' hd
#'
#' @param var_model
#' @param plot
#' @param plot_res
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @return
hd.var_model = function(
  var_model,
  plot = T,
  plot_res = T
)

{

  # Get outputs
  list2env(var_model, envir = environment())
  list2env(var_model$model_data, envir = environment())

  # Step 1 - compute structural coefficient matrices:
  Phi = compute_Phi(lags, K, A_comp, const, n.ahead=N)
  B_0 = identify_chol(Sigma_res)
  Theta = compute_Theta(Phi, B_0)

  # Step 2 - compute the structural shocks:
  w = res %*% t(B_0)

  # Step 3 - match structural shocks with appropriate impulse response weight:
  y_hat_dt = data.table::rbindlist(
    lapply(1:N, function(t) { # loop over time periods
      terms_to_sum = lapply(1:t, function(s) {
        sapply(1:K, function(k) Theta[[s]][k,] * w[t-s+1,])
      })
      y_hat_t = Reduce(`+`, terms_to_sum)
      colnames(y_hat_t) = var_names
      y_hat_t = data.table::as.data.table(t(y_hat_t))[,c("k","t"):=.(var_names, t)]
      return(y_hat_t)
    })
  )[order(k,t)]

  # Step 4 - tidy up:
  y_hat_dt[,actual:=detrend(y[,k]),by=k]
  y_hat_dt[,fitted:=rowSums(.SD),.SDcols=var_names]
  y_hat_dt[,residual:=actual-fitted]

  # Plot
  if (plot==T) {
    chart_data = data.table::melt(y_hat_dt, measure.vars = c("residual", var_names))
    if (plot_res==T) {
      p = ggplot2::ggplot(chart_data) +
        ggplot2::geom_col(ggplot2::aes(x=t, y=value, fill=variable)) +
        ggplot2::geom_line(ggplot2::aes(x=t, y=actual)) +
        ggplot2::facet_wrap(k ~ .)
      p
    } else {
      p = ggplot2::ggplot(chart_data[variable!="residual"]) +
        ggplot2::geom_col(ggplot2::aes(x=t, y=value, fill=variable)) +
        ggplot2::geom_line(ggplot2::aes(x=t, y=fitted)) +
        ggplot2::facet_wrap(k ~ .)
      p
    }
  }

  return(y_hat_dt)

}

hd <- function(var_model, plot = T, plot_res = T) {
  UseMethod("hd", var_model)
}

#' fevd
#'
#' @param var_model
#' @param n.ahead
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @return
#'
#' @description Function used to compute forecast error variance decomposition as in Kilian, Luetkepohl
#'

fevd.var_model = function(
  var_model,
  n.ahead=10,
  plot=T,
  ylab="Variance contribution",
  xlab="Forecast horizon",
  title=NULL,
  theme=ggplot2::theme_bw()
)

{

  # Get outputs
  list2env(var_model, envir = environment())
  list2env(var_model$model_data, envir = environment())

  # Compute MSPE
  Phi = compute_Phi(lags, K, A_comp, const, n.ahead)
  B_0 = identify_chol(Sigma_res)
  Theta = compute_Theta(Phi, B_0)
  theta_kj_sq = compute_theta_kj_sq(Theta, n.ahead)
  mspe = compute_mspe(Theta, n.ahead)

  # Compute percentage contributions
  fevd_list = lapply(1:K, function(k) {
    t(sapply(1:length(mspe), function(h) {
      mspe_k = mspe[[h]][k,k]
      theta_k_sq = theta_kj_sq[[h]][,k]
      fevd = theta_k_sq/mspe_k
    }))
  })
  names(fevd_list) = var_names

  # Tidy
  fevd_tidy = data.table::rbindlist(
    lapply(1:length(fevd_list), function(k) {
      fevd_k = data.table::melt(data.table::data.table(fevd_list[[k]])[,h:=.I], id.vars = "h", variable.name = "j")
      fevd_k[,k:=names(fevd_list)[k]]
      data.table::setcolorder(fevd_k, c("k", "j", "h"))
    })
  )

  # Plot
  if (plot==T) {
    p = ggplot2::ggplot(data=fevd_tidy) +
      ggplot2::geom_area(ggplot2::aes(x=h, y=value, fill=j)) +
      ggplot2::facet_wrap(k ~ .) +
      ggplot2::scale_x_continuous(
        expand=c(0,0)
      ) +
      ggplot2::scale_fill_discrete(
        name = "Shock"
      ) +
      ggplot2::labs(
        x =xlab,
        y=ylab,
        title = title
      ) +
      theme
    # print(p)
  }

  return(
    list(
      fevd_tidy=fevd_tidy,
      plot = p
    )
  )

}

fevd <- function(
  var_model,
  n.ahead=10,
  plot=T,
  ylab="Variance contribution",
  xlab="Forecast horizon",
  title=NULL,
  theme=ggplot2::theme_bw()
) {
  UseMethod("fevd", var_model)
}

#' irf
#'
#' @param var_model
#' @param imp
#' @param structural
#' @param n_ahead
#' @param size
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @return
#'
#' @author Patrick Altmeyer
irf.var_model <- function(
  var_model,
  imp,
  structural = T,
  n_ahead = 10,
  size = 1,
  bootstrap = T,
  n_bootstrap = 1000,
  parametric = F,
  ci = .95,
  plot = T,
  cum = T
) {

  # Get outputs
  list2env(var_model, envir = environment())
  list2env(var_model$model_data, envir = environment())

  # Compute IRF
  compute_IRF = function(
    var_model,
    imp,
    structural,
    n_ahead ,
    size
  ) {

    # Get outputs
    list2env(var_model, envir = environment())
    list2env(var_model$model_data, envir = environment())

    # Pre-processing
    imp = ifelse(class(imp) == "character", which(var_names == imp), imp)
    # Choleski decomposition - lower triangular:
    B_0 = identify_chol(Sigma_res)

    # Initialization
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
    X_ = matrix(rep(0,K * lags), nrow=K * lags)
    names(X_) = colnames(X)[(1+constant):ncol(X)]
    # Coefficients:
    A_trans = t(A[(1+constant):nrow(A),])
    # Container for state responses:
    irf = matrix(NA, nrow = n_ahead+1, ncol=K)
    colnames(irf) = var_names
    # Initial shocks at time zero:
    Y_h = A_trans %*% X_ + shocks
    irf[1,] = Y_h
    # Update vector of regressors (lagged values of y):
    X_ = c(Y_h,X_)[1:(K * lags)]
    names(X_) = colnames(X)[(1+constant):ncol(X)]

    # Recursion
    for (h in 2:(n_ahead+1)) {

      Y_h = A_trans %*% X_
      # Update vector of regressors:
      X_ = c(Y_h,X_)[1:(K * lags)]
      names(X_) = colnames(X)[(1+constant):ncol(X)]
      # Update IRF
      irf[h,] = Y_h

    }

    # Tidy up
    irf = data.table::melt(data.table::as.data.table(irf), measure.vars = colnames(irf), value.name = "irf")
    irf[,cum_irf:=cumsum(irf),by=variable]
    irf[,h:=1:.N,by=variable]
    irf = data.table::melt(irf, id.vars = c("variable", "h"), variable.name = "type")

    return(irf)

  }

  irf_output = compute_IRF(
    var_model,
    imp,
    structural,
    n_ahead,
    size
  )

  # Bootstrapped confidence intervals
  if (bootstrap==T) {

    bs = lapply(1:n_bootstrap, function(x) {

      # STEP 1 - sample from residual matrix with replacement
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
      Y_star <- matrix(y[1:lags,],ncol=K) # pre-sample values actually realized

      # Recursion
      for (i in 1:nrow(res_star)) {

        # Regressor
        if (constant==T) {
          X_ = c(1,t(Y_star[nrow(Y_star):(nrow(Y_star)-lags+1),]))
        } else {
          X_ = c(t(Y_star[nrow(Y_star):(nrow(Y_star)-lags+1),]))
        }

        Y_hat = X_ %*% A + res_star[i,] # 1-step ahead prediction
        # Update y_star:
        Y_star = rbind(Y_star, Y_hat)

      }

      # STEP 3 - estimate VAR(p) for simulated data
      var_model_star = vareg(Y_star,lags, constant = constant)

      # STEP 4 - estimate IRF using new coefficients
      irf_star = compute_IRF(var_model = var_model_star,
                             imp = imp,
                             n_ahead = n_ahead,
                             structural = structural,
                             size = size)

      # Prepare
      irf_star = data.table::as.data.table(irf_star)
      # irf_star[,h:=1:.N,by=variable]
      irf_star[,n_bs:=x]

      return(irf_star)

    })
    bs = data.table::rbindlist(bs)

    # STEP 5 - compute bands
    ci_bands = bs[,.(upper=stats::quantile(value, probs = 1-p),
                     lower=stats::quantile(value, probs = p)),
                  by = .(variable, h, type)]

    irf_output = merge(irf_output, ci_bands, on=c("variable", "h", "type"))

  }

  # Plot
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
              "var_model" = var_model,
              "imp" = imp,
              "cum" = cum,
              "structural" = structural,
              "size" = size))

}

irf <- function(
  var_model,
  imp,
  structural = T,
  n_ahead = 10,
  size = 1,
  bootstrap = T,
  n_bootstrap = 1000,
  parametric = F,
  ci = .95,
  plot = T,
  cum = T
) {
  UseMethod("irf", var_model)
}
