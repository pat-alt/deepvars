---
author: Patrick Altmeyer
format: gfm
project:
  lib-dir: site_libs
  output-dir: ../docs
  type: website
site:
  navbar:
    background: primary
    left:
    - href: index.qmd
      text: Home
    - href: rnn_torch.qmd
      text: RNN in `torch` for `deepvars`
    type: dark
  title: docs
title: RNN in Torch
toc-title: On this page
website:
  navbar:
    background: primary
    left:
    - href: index.qmd
      text: Home
    - href: rnn_torch.qmd
      text: RNN in `torch` for `deepvars`
    type: dark
  title: docs
---

## Overview

This tutorial demonstrates how the Recurrent Neural Network underlying
Deep VAR is set up using [torch for R](https://torch.mlverse.org/). It
heavily draws on ideas and code presented in this great
[tutorial](https://blogs.rstudio.com/ai/posts/2021-03-11-forecasting-time-series-with-torch_2/)
from the RStudio AI blog.

## Data input

<div class="cell">

``` r
train_val_test_split <- function(data, train_size=0.8) {
  N <- nrow(data)
  end_train <- round(train_size * N)
  end_val <- end_train + round((N - end_train)/2)
  data_train <- data[1:end_train,] |> as.matrix()
  data_val <- data[(end_train+1):end_val,] |> as.matrix()
  data_test <- data[(end_val+1):N,] |> as.matrix()
  return(list(train=data_train, val=data_val, test=data_test))
}
```

</div>

<div class="cell">

``` r
library(deepvars)
library(data.table)
data("canada")
dt <- data.table(canada)
var_cols = colnames(dt)[2:ncol(dt)]
dt[,(var_cols) := lapply(.SD, function(i) c(0,diff(i))), .SDcols=var_cols]
splits_ <- train_val_test_split(dt[,-1], train_size = 0.5)
df_train <- splits_$train
df_val <- splits_$val
df_test <- splits_$test
```

</div>

Recall that the neural network that we are aiming to build takes as its
input its own *p* lags as well as *p* lags of all other variables in the
system.

<div class="cell">

``` r
library(torch)

dvar_dataset <- dataset(
  name = "dvar_dataset",
  
  initialize = function(X, response, lags, n_ahead, sample_frac = 1) {
    
    self$lags <- lags
    self$n_ahead <- n_ahead
    self$response <- response
    self$train_mean <- colMeans(X)
    self$train_sd <- sapply(1:ncol(X), function(i) sd(X[,i]))
    self$X <- torch_tensor(t((t(X) - self$train_mean)/self$train_sd)) # of dimension (D x T)
    
    n <- dim(self$X)[1] - self$lags - self$n_ahead + 1
    
    self$starts <- sort(sample.int(
      n = n,
      size = round(n * sample_frac)
    ))
    
  },
  
  .getitem = function(i) {
    
    start <- self$starts[i]
    end <- start + self$lags - 1
    pred_length <- self$n_ahead
    
    list(
      X = self$X[start:end,],
      y = self$X[(end + 1):(end + pred_length),self$response]
    )
    
  },
  
  .length = function() {
    length(self$starts) 
  }
)
```

</div>

<div class="cell">

``` r
set.seed(123)
response_var_idx <- 1
lags <- 6
n_ahead <- 12
train_ds <- dvar_dataset(df_train, response_var_idx, lags, n_ahead = n_ahead, sample_frac = 0.5)

batch_size <- 30
train_dl <- train_ds %>% dataloader(batch_size = batch_size, shuffle = TRUE)

valid_ds <- dvar_dataset(df_val, response_var_idx, lags, n_ahead=n_ahead, sample_frac = 0.5)
valid_dl <- valid_ds %>% dataloader(batch_size = batch_size)

test_ds <- dvar_dataset(df_test, response_var_idx, lags, n_ahead=n_ahead)
test_dl <- test_ds %>% dataloader(batch_size = 1)
```

</div>

Let’s do a quick sanity check to see if the dimensions check out:

<div class="cell">

``` r
train_ds[1]
```

<div class="cell-output-stdout">

    $X
    torch_tensor
     0.1020 -1.1904  0.9092 -0.4763
     0.9909  0.5094  1.8195 -0.4147
     1.1777  1.1057  1.2633  0.2022
     0.6616 -0.9201  1.6707 -0.4969
    -0.6957 -2.2618 -0.5863  0.5517
    -1.3460 -1.2138  3.2111  1.9088
    [ CPUFloatType{6,4} ]

    $y
    torch_tensor
    -2.0908
    -2.8897
    -2.9660
    -1.4641
     0.0895
     1.1206
     1.4424
     0.0236
    -0.4609
     0.1643
     0.6791
    -0.0035
    [ CPUFloatType{12} ]

</div>

</div>

This looks like what we expected: `X` is a (*D*×*p*) tensor and *y* is
just the single output.

<div class="cell">

``` r
in_idx <- train_ds$starts[1]:(train_ds$starts[1]+train_ds$lags-1)
out_idx <- c(max(in_idx)+1,response_var_idx)
X_train <- t((t(df_train) - colMeans(df_train)) / sapply(1:ncol(df_train), function(i) sd(df_train[,i])))
X_train[in_idx,]
```

<div class="cell-output-stdout">

                  e       prod         rw          U
    [1,]  0.1019674 -1.1904140  0.9092028 -0.4763473
    [2,]  0.9908521  0.5093805  1.8195315 -0.4146620
    [3,]  1.1776523  1.1057034  1.2633140  0.2021906
    [4,]  0.6615854 -0.9200533  1.6706661 -0.4969091
    [5,] -0.6956522 -2.2618121 -0.5862564  0.5517404
    [6,] -1.3459703 -1.2137674  3.2111416  1.9088162

</div>

``` r
X_train[out_idx[1]:(out_idx[1]+n_ahead-1),out_idx[2]]
```

<div class="cell-output-stdout">

     [1] -2.090789787 -2.889711226 -2.966011884 -1.464126631  0.089478145
     [6]  1.120601201  1.442417457  0.023572063 -0.460867622  0.164265269
    [11]  0.679070086 -0.003545797

</div>

</div>

What about the mini-batch? We see that `X` is of the desired dimension
`(batch_size, n_timesteps, num_features)`.

<div class="cell">

``` r
length(train_dl)
```

<div class="cell-output-stdout">

    [1] 1

</div>

``` r
b <- train_dl %>% dataloader_make_iter() %>% dataloader_next()
b
```

<div class="cell-output-stdout">

    $X
    torch_tensor
    (1,.,.) = 
      1.4424 -0.1345 -0.7919 -1.0932
      0.0236  0.8412  0.4699 -1.0315
     -0.4609  1.5454  0.1831  0.1405
      0.1643  1.0204 -0.5691  0.4078
      0.6791 -0.7397 -0.8391 -0.3530
     -0.0035  1.1853 -0.1828 -0.2707

    (2,.,.) = 
     -2.9660  0.4020  0.2097  3.6360
     -1.4641 -0.1386  0.6799  1.1686
      0.0895  1.6895 -1.6889 -0.7025
      1.1206  0.4061 -0.2826 -0.4147
      1.4424 -0.1345 -0.7919 -1.0932
      0.0236  0.8412  0.4699 -1.0315

    (3,.,.) = 
      1.1206  0.4061 -0.2826 -0.4147
      1.4424 -0.1345 -0.7919 -1.0932
      0.0236  0.8412  0.4699 -1.0315
     -0.4609  1.5454  0.1831  0.1405
      0.1643  1.0204 -0.5691  0.4078
      0.6791 -0.7397 -0.8391 -0.3530

    (4,.,.) = 
      0.6616 -0.9201  1.6707 -0.4969
     -0.6957 -2.2618 -0.5863  0.5517
     -1.3460 -1.2138  3.2111  1.9088
     -2.0908  0.1093  1.7242  1.0247
     -2.8897 -1.1323  0.8589  3.2865
    ... [the output was truncated (use n=-1 to disable)]
    [ CPUFloatType{12,6,4} ]

    $y
    torch_tensor
    Columns 1 to 10-0.0456  0.8483  0.6847  0.6916  0.8213  0.2048 -0.3137 -0.0030  0.1853  1.1057
    -0.4609  0.1643  0.6791 -0.0035 -0.0456  0.8483  0.6847  0.6916  0.8213  0.2048
    -0.0035 -0.0456  0.8483  0.6847  0.6916  0.8213  0.2048 -0.3137 -0.0030  0.1853
    -1.4641  0.0895  1.1206  1.4424  0.0236 -0.4609  0.1643  0.6791 -0.0035 -0.0456
     0.8213  0.2048 -0.3137 -0.0030  0.1853  1.1057  0.6218  1.1774  0.5537  0.0394
     0.1853  1.1057  0.6218  1.1774  0.5537  0.0394 -0.2996  0.4384  0.9216 -0.6563
    -0.3137 -0.0030  0.1853  1.1057  0.6218  1.1774  0.5537  0.0394 -0.2996  0.4384
     1.4424  0.0236 -0.4609  0.1643  0.6791 -0.0035 -0.0456  0.8483  0.6847  0.6916
    -2.9660 -1.4641  0.0895  1.1206  1.4424  0.0236 -0.4609  0.1643  0.6791 -0.0035
     0.6916  0.8213  0.2048 -0.3137 -0.0030  0.1853  1.1057  0.6218  1.1774  0.5537
    -2.0908 -2.8897 -2.9660 -1.4641  0.0895  1.1206  1.4424  0.0236 -0.4609  0.1643
     0.0236 -0.4609  0.1643  0.6791 -0.0035 -0.0456  0.8483  0.6847  0.6916  0.8213

    Columns 11 to 12 0.6218  1.1774
    -0.3137 -0.0030
     1.1057  0.6218
     0.8483  0.6847
    -0.2996  0.4384
    -0.1092 -0.0666
     0.9216 -0.6563
     0.8213  0.2048
    -0.0456  0.8483
     0.0394 -0.2996
     0.6791 -0.0035
     0.2048 -0.3137
    [ CPUFloatType{12,12} ]

</div>

</div>

## Model

<div class="cell">

``` r
model <- nn_module(
  
  initialize = function(type="lstm", input_size, hidden_size, linear_size, output_size,
                        num_layers = 2, dropout = 0.25, linear_dropout = 0.25) {
    
    self$type <- type
    self$num_layers <- num_layers
    self$linear_dropout <- linear_dropout
    
    self$rnn <- if (self$type == "gru") {
      nn_gru(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        dropout = dropout,
        batch_first = TRUE
      )
    } else {
      nn_lstm(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        dropout = dropout,
        batch_first = TRUE
      )
    }
    
    self$mlp <- nn_sequential(
      nn_linear(hidden_size, linear_size),
      nn_relu(),
      nn_dropout(linear_dropout),
      nn_linear(linear_size, output_size)
    )
    
  },
  
  forward = function(x) {
    
    x <- self$rnn(x)
    x[[1]][ ,-1, ..] %>% 
      self$mlp()
    
  }
  
)
```

</div>

<div class="cell">

``` r
D <- dim(train_ds[1]$X)[2]
net <- model(input_size = D, hidden_size = 32, linear_size = 128, output_size = n_ahead)
device <- torch_device(if (cuda_is_available()) "cuda" else "cpu")
net <- net$to(device = device)
```

</div>

## Training

<div class="cell">

``` r
optimizer <- optim_adam(net$parameters, lr = 0.001)

num_epochs <- 30

train_batch <- function(b) {
  
  optimizer$zero_grad() # in
  output <- net(b$X$to(device = device))
  target <- b$y$to(device = device)
  
  loss <- nnf_mse_loss(output, target)
  loss$backward()
  optimizer$step()
  
  loss$item()
}

valid_batch <- function(b) {
  
  output <- net(b$X$to(device = device))
  target <- b$y$to(device = device)
  
  loss <- nnf_mse_loss(output, target)
  loss$item()
  
}

for (epoch in 1:num_epochs) {
  
  net$train()
  train_loss <- c()
  
  coro::loop(for (b in train_dl) {
    loss <- train_batch(b)
    train_loss <- c(train_loss, loss)
  })
  
  cat(sprintf("\nEpoch %d, training: loss: %3.5f \n", epoch, mean(train_loss)))
  
  net$eval()
  valid_loss <- c()
  
  coro::loop(for (b in valid_dl) {
    loss <- valid_batch(b)
    valid_loss <- c(valid_loss, loss)
  })
  
  cat(sprintf("\nEpoch %d, validation: loss: %3.5f \n", epoch, mean(valid_loss)))
}
```

<div class="cell-output-stdout">


    Epoch 1, training: loss: 0.64473 

    Epoch 1, validation: loss: 0.79544 

    Epoch 2, training: loss: 0.64353 

    Epoch 2, validation: loss: 0.78293 

    Epoch 3, training: loss: 0.63548 

    Epoch 3, validation: loss: 0.77065 

    Epoch 4, training: loss: 0.63260 

    Epoch 4, validation: loss: 0.75832 

    Epoch 5, training: loss: 0.62000 

    Epoch 5, validation: loss: 0.74583 

    Epoch 6, training: loss: 0.61640 

    Epoch 6, validation: loss: 0.73339 

    Epoch 7, training: loss: 0.60513 

    Epoch 7, validation: loss: 0.72109 

    Epoch 8, training: loss: 0.59723 

    Epoch 8, validation: loss: 0.70906 

    Epoch 9, training: loss: 0.59636 

    Epoch 9, validation: loss: 0.69679 

    Epoch 10, training: loss: 0.58609 

    Epoch 10, validation: loss: 0.68437 

    Epoch 11, training: loss: 0.57969 

    Epoch 11, validation: loss: 0.67186 

    Epoch 12, training: loss: 0.57611 

    Epoch 12, validation: loss: 0.65910 

    Epoch 13, training: loss: 0.57321 

    Epoch 13, validation: loss: 0.64597 

    Epoch 14, training: loss: 0.55939 

    Epoch 14, validation: loss: 0.63253 

    Epoch 15, training: loss: 0.57057 

    Epoch 15, validation: loss: 0.61889 

    Epoch 16, training: loss: 0.54712 

    Epoch 16, validation: loss: 0.60493 

    Epoch 17, training: loss: 0.54523 

    Epoch 17, validation: loss: 0.59066 

    Epoch 18, training: loss: 0.53944 

    Epoch 18, validation: loss: 0.57578 

    Epoch 19, training: loss: 0.53803 

    Epoch 19, validation: loss: 0.56049 

    Epoch 20, training: loss: 0.53603 

    Epoch 20, validation: loss: 0.54494 

    Epoch 21, training: loss: 0.51970 

    Epoch 21, validation: loss: 0.52912 

    Epoch 22, training: loss: 0.51931 

    Epoch 22, validation: loss: 0.51338 

    Epoch 23, training: loss: 0.51753 

    Epoch 23, validation: loss: 0.49760 

    Epoch 24, training: loss: 0.50235 

    Epoch 24, validation: loss: 0.48188 

    Epoch 25, training: loss: 0.49555 

    Epoch 25, validation: loss: 0.46675 

    Epoch 26, training: loss: 0.50679 

    Epoch 26, validation: loss: 0.45367 

    Epoch 27, training: loss: 0.50799 

    Epoch 27, validation: loss: 0.44272 

    Epoch 28, training: loss: 0.50215 

    Epoch 28, validation: loss: 0.43401 

    Epoch 29, training: loss: 0.49051 

    Epoch 29, validation: loss: 0.42748 

    Epoch 30, training: loss: 0.48860 

    Epoch 30, validation: loss: 0.42310 

</div>

</div>

## Evaluation

Below we see the 12-step prediction

<div class="cell">

``` r
net$eval()

test_preds <- vector(mode = "list", length = length(test_dl))

i <- 1

coro::loop(for (b in test_dl) {
  
  input <- b$X
  output <- net(input$to(device = device))
  preds <- as.numeric(output)
  
  test_preds[[i]] <- preds
  i <<- i + 1
  
})

y_hat <- test_preds[[1]] * train_ds$train_sd[response_var_idx] + train_ds$train_mean[response_var_idx]
y_hat <- data.table(y_hat)[,id:=1:.N][,variable:=colnames(df_train)[response_var_idx]]
setkey(y_hat, id, variable)

library(data.table)
y_true <- data.table(rbind(df_val,df_test))
y_true <- y_true[,..response_var_idx]
y_true[,id:=(-nrow(df_val)-lags+1):(-nrow(df_val)-lags+y_true[,.N])]
y_true <- melt(y_true, id.vars = "id", value.name = "y")
setkey(y_true, id, variable)

dt_plot <- melt(y_hat[y_true],id.vars = c("id","variable"), variable.name = "type")
dt_plot[type=="y_hat" & id==0, value:=dt_plot[type=="y" & id==0]$value]
library(ggplot2)

ggplot(dt_plot[id<=n_ahead], aes(x=id, y=value, colour=type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(name="Type:", values=c("blue", "red"), labels=c("Prediction", "Actual")) +
  labs(
    title = sprintf("Variable: %s", colnames(df_train)[response_var_idx]),
    x = "Time",
    y = "Value"
  )
```

<div class="cell-output-display">

![](rnn_torch_files/figure-gfm/unnamed-chunk-11-1.png)

</div>

</div>
