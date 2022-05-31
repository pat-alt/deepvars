library(foreach)
library(data.table)
library(doParallel)

rolling_window_errors <- function(dt, w_size=150, n_ahead=12) {

  # Number of windows and window size
  n_windows = nrow(dt) - w_size - n_ahead
  var_cols <- colnames(dt)[2:ncol(dt)]

  # Deep VAR params:
  num_units <- 64
  num_layers <- formals(deepvareg)$num_layers
  dropout <- formals(deepvareg)$p_drop_out
  epochs <- 800

  # Cluster:
  no_cores <- detectCores() - 2
  cl <- makeCluster(no_cores, type="FORK")
  registerDoParallel(cl)

  # Rolling Window Loop:
  fcst <- foreach(i = 1:n_windows, .combine = rbind) %dopar% {

    message(sprintf("Window %i out of %i", i, n_windows))

    # SETUP
    dt_in <- dt[i:(w_size + i - 1)]
    dt_out <- dt[(w_size + i):(w_size + i + n_ahead - 1)][,id:=1:.N]
    dt_out_l <- melt(dt_out[,-1], id.vars = "id", value.name = "y")
    setkey(dt_out_l, id, variable)

    # RUNNING MODELS
    # Choosing lags:
    max_lags <- 12
    lags <- lag_order(dt_in, max_lag = max_lags)$p

    # VAR fitting:
    var_model <- vareg(dt_in, lags = lags)

    # Deep VAR fitting:
    deepvar_model <- deepvareg(
      dt_in,
      lags = lags,
      num_units = num_units,
      epochs = epochs,
      size_ensemble = 1
    )
    # Threshold VAR
    tv_model <- tsDyn::TVAR(dt_in[,-1], include = "const", lag=lags, nthresh=2, trim=0.1)

    # FORECASTS
    # VAR:
    fcst_var <- deepvars::forecast(var_model, n.ahead = n_ahead)$fcst[-1,-1][,id:=1:.N]
    fcst_var <- melt(fcst_var, id.vars = "id", value.name = "y_hat")[,model:="var"]
    setkey(fcst_var, id, variable)
    # Deep VAR:
    fcst_dvar <- deepvars::forecast(deepvar_model, n.ahead = n_ahead)$fcst[-1,-1][,id:=1:.N]
    fcst_dvar <- melt(fcst_dvar, id.vars = "id", value.name = "y_hat")[,model:="dvar"]
    setkey(fcst_dvar, id, variable)
    # Random Walk:
    fcst_rw <- rbind(dt_in[nrow(dt_in)],dt_out[1:(nrow(dt_out)-1)],fill=TRUE)[,id:=1:.N]
    fcst_rw <- melt(fcst_rw[,-1], id.vars="id", value.name = "y_hat")[,model:="rw"]
    setkey(fcst_rw, id, variable)
    # Threshold VAR
    fcst_tv <- data.table(predict(tv_model, n.ahead = n_ahead))[,id:=1:.N]
    fcst_tv <- melt(fcst_tv, id.vars="id", value.name = "y_hat")[,model:="tv"]
    setkey(fcst_tv, id, variable)

    fcst <- rbind(fcst_var, fcst_dvar, fcst_rw, fcst_tv)
    fcst <- dt_out_l[fcst]
    fcst[,sqerror:=(y-y_hat)^2]
    fcst[,window:=i]

    return(fcst)

    gc()

  }

  return(fcst)

}
