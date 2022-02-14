---
title: "Deep VARs"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Deep VARs}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




```r
rm(list=ls())
library(deepvars)
```

## Reduced-form VAR 

Recall the functional from of the reduced-form VAR($p$) with $K$ variables and $p$ lags (with $m \in [1,p]$) 

$$
\begin{aligned}
&& y_t&=A_1 y_{t-1} + A_2 y_{t-2} + ... + A_p y_{t-p} + u_t \\
\end{aligned}
$$

where $y_{t-m}$ are $(K \times 1)$ vectors , $A_m$ are $(K \times K)$ matrices of coefficients and $u_t$ is a $(K \times 1)$ vector of residuals. 

## Deep VAR

The deep VAR ...

## A simple benchmark


```r
dt = data.table::data.table(deepvars::canada)
var_cols = colnames(dt)[2:ncol(dt)]
```

The underlying time series in levels look non-stationary 


```r
dt[,(var_cols) := lapply(.SD, function(i) c(0,diff(i))), .SDcols=var_cols]
chart_data = dt
chart_data = data.table::melt(chart_data, id.vars = "date")

ggplot2::ggplot(chart_data) +
  ggplot2::geom_line(ggplot2::aes(y=value, x=date)) +
  ggplot2::facet_wrap(variable ~ ., scales = "free_y") +
  ggplot2::theme_bw() +
  ggplot2::labs(
    x="Date",
    y="Value"
  )
```

![Time series in differences.](/private/var/folders/99/2fj_h1qs3gj3x8nqmpx20mrc0000gq/T/Rtmpy94xkL/preview-129d723537d7a.dir/deepvars_files/figure-html/after-diff-1.png)

Splitting data into a training and test sample ...


```r
train_test_split <- split_sample(dt)
train_data <- train_test_split$train_data
test_data <- train_test_split$test_data
```

Select lag length ...


```r
lags <- lag_order(train_data[,.SD,.SDcols=var_cols], max_lag = 12)$p
```

Fit the two models ...


```r
# VAR
var_model <- vareg(train_data, lags = lags)
# Deep VAR
deepvar_model <- deepvareg(train_data, lags=lags, num_units = 100, epochs=500)
```

### 1-step ahead predictions

#### In-sample


```r
y_true <- var_model$y_train
```


```r
pred_var <- predict(var_model)
plot(pred_var, y_true = y_true)
```

![Predictions from VAR.](/private/var/folders/99/2fj_h1qs3gj3x8nqmpx20mrc0000gq/T/Rtmpy94xkL/preview-129d723537d7a.dir/deepvars_files/figure-html/pred-var-1.png)


```r
pred_deepvar <- predict(deepvar_model)
plot(pred_deepvar, y_true = y_true)
```

![Predictions from MLSTM.](/private/var/folders/99/2fj_h1qs3gj3x8nqmpx20mrc0000gq/T/Rtmpy94xkL/preview-129d723537d7a.dir/deepvars_files/figure-html/pred-mlstm-1.png)

#### Out-of-sample


```r
X_test <- prepare_test_data(train_test_split, lags=lags)$X_test
y_test <- prepare_test_data(train_test_split, lags=lags)$y_test
```



```r
pred_var <- predict(var_model, X=X_test)
plot(pred_var, y_true = y_test)
```

![Out-of-sample predictions from VAR.](/private/var/folders/99/2fj_h1qs3gj3x8nqmpx20mrc0000gq/T/Rtmpy94xkL/preview-129d723537d7a.dir/deepvars_files/figure-html/out-pred-var-1.png)


```r
pred_dvar <- predict(deepvar_model, X=X_test)
plot(pred_dvar, y_true = y_test)
```

![Out-of-sample predictions from DVAR.](/private/var/folders/99/2fj_h1qs3gj3x8nqmpx20mrc0000gq/T/Rtmpy94xkL/preview-129d723537d7a.dir/deepvars_files/figure-html/out-pred-dvar-1.png)

#### Cumulative loss


```r
cum_loss_var <- cum_loss(var_model)$cum_loss[,type:="var"]
cum_loss_dvar <- cum_loss(deepvar_model)$cum_loss[,type:="deepvar"]
dt_plot <- rbind(cum_loss_dvar, cum_loss_var)
# dt_plot[,date:=train_data$date[date]]
dt_plot[,type:=factor(type)]
levels(dt_plot$type) <- c("Deep VAR", "VAR")
ggplot2::ggplot(data=dt_plot, ggplot2::aes(x=date, y=value, colour=type)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~variable, scales = "free_y") +
  ggplot2::scale_color_discrete(name="Model:") +
  ggplot2::labs(
      x="Date",
      y="Squared error"
    )
```

![Comparison of cumulative loss for the two models (training sample).](/private/var/folders/99/2fj_h1qs3gj3x8nqmpx20mrc0000gq/T/Rtmpy94xkL/preview-129d723537d7a.dir/deepvars_files/figure-html/cum-loss-1.png)


```r
cum_loss_var <- cum_loss(var_model, X=X_test, y=y_test)$cum_loss[,type:="var"]
cum_loss_dvar <- cum_loss(deepvar_model, X=X_test, y=y_test)$cum_loss[,type:="deepvar"]
dt_plot <- rbind(cum_loss_dvar, cum_loss_var)
# dt_plot[,date:=test_data$date,by=.(variable, type)]
dt_plot[,type:=factor(type)]
ggplot2::ggplot(data=dt_plot, ggplot2::aes(x=date, y=value, colour=type)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~variable, scales = "free_y") +
  ggplot2::scale_color_discrete(name="Model:") +
  ggplot2::labs(
      x="Date",
      y="Squared error"
    )
```

![Comparison of cumulative loss for the two models (test sample).](/private/var/folders/99/2fj_h1qs3gj3x8nqmpx20mrc0000gq/T/Rtmpy94xkL/preview-129d723537d7a.dir/deepvars_files/figure-html/cum-loss-out-1.png)

#### RMSE


```r
rmse_var <- rbind(
  rmse(var_model)[,model:="var"][,sample:="train"],
  rmse(var_model, X=X_test, y=y_test)[,model:="var"][,sample:="test"]
)
rmse_dvar <- rbind(
  rmse(deepvar_model)[,model:="deepvar"][,sample:="train"],
  rmse(deepvar_model, X=X_test, y=y_test)[,model:="deepvar"][,sample:="test"]
)
tab_rmse <- rbind(rmse_var, rmse_dvar)
tab_rmse <- data.table::dcast(tab_rmse, sample + variable ~ model, value.var = "value")
knitr::kable(
  tab_rmse, 
  col.names = c("Sample", "Variable", "DVAR", "VAR"),
  digits = 5
) 
```



|Sample |Variable |    DVAR|     VAR|
|:------|:--------|-------:|-------:|
|test   |e        | 0.35709| 1.52839|
|test   |prod     | 0.46324| 2.51180|
|test   |rw       | 1.16701| 2.10415|
|test   |U        | 0.24542| 1.16071|
|train  |e        | 0.02455| 0.13340|
|train  |prod     | 0.07314| 0.23076|
|train  |rw       | 0.06273| 0.25987|
|train  |U        | 0.02797| 0.10419|

### h-step ahead forecasts

#### Charts


```r
n_ahead <- nrow(y_test)
fcst_var <- forecast(var_model, n.ahead = n_ahead)
plot(fcst_var, y_true = y_test)
```

![Forecasts from VAR.](/private/var/folders/99/2fj_h1qs3gj3x8nqmpx20mrc0000gq/T/Rtmpy94xkL/preview-129d723537d7a.dir/deepvars_files/figure-html/fcst-var-1.png)


```r
fcst_dvar <- forecast(deepvar_model, n.ahead = n_ahead)
plot(fcst_dvar, y_true = y_test)
```

![Forecasts from Deep VAR.](/private/var/folders/99/2fj_h1qs3gj3x8nqmpx20mrc0000gq/T/Rtmpy94xkL/preview-129d723537d7a.dir/deepvars_files/figure-html/fcst-dvar-1.png)

#### RMSFE 

In terms of the Root Mean Squared Forecasting Error the Deep VAR is clearly dominating:


```r
tab_rmsfe <- rbind(
  rmsfe(fcst_var, y_true=y_test)[,model:="VAR"],
  rmsfe(fcst_dvar, y_true=y_test)[,model:="DVAR"]
)
tab_rmsfe <- data.table::dcast(tab_rmsfe, variable ~ model, value.var = "value")
knitr::kable(
  tab_rmsfe, 
  col.names = c("Variable", "DVAR", "VAR"),
  digits = 5
) 
```



|Variable |    DVAR|     VAR|
|:--------|-------:|-------:|
|U        | 0.24303| 1.07060|
|e        | 0.36467| 1.61887|
|prod     | 0.95375| 2.05283|
|rw       | 1.20869| 2.76801|

#### Correlations

With respect to correlations between forecasts and actual outcomes there is no clear winner. The VAR appear to perform better for unemployment `U`, while the Deep VAR clearly dominates for production `prod`.


```r
tab_cor_fcst <- rbind(
  cor_fcst(fcst_var, y_true=y_test)[,model:="VAR"],
  cor_fcst(fcst_dvar, y_true=y_test)[,model:="DVAR"]
)
tab_cor_fcst <- data.table::dcast(tab_cor_fcst, variable ~ model, value.var = "value")
knitr::kable(
  tab_cor_fcst, 
  col.names = c("Variable", "DVAR", "VAR"),
  digits = 5
) 
```



|Variable |     DVAR|      VAR|
|:--------|--------:|--------:|
|U        | -0.30654|  0.38934|
|e        | -0.21578|  0.01346|
|prod     |  0.50568|  0.14389|
|rw       | -0.11030| -0.25109|


