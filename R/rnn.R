# Prepare data for RNN: ----
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

# Perpare the RNN: ----
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

# Training: ----
train_batch <- function(rnn, b, loss, optim) {

  optim$zero_grad() # in
  output <- rnn(b$X$to(device = device))
  target <- b$y$to(device = device)

  l <- loss(output, target)
  l$backward()
  optim$step()

  l$item()
}

valid_batch <- function(rnn, b, loss) {

  output <- rnn(b$X$to(device = device))
  target <- b$y$to(device = device)

  l <- loss(output, target)
  l$item()

}

forward_rnn <- function(
  rnn, train_dl, valid_dl,
  loss=torch::nnf_mse_loss,
  optim=torch::optim_adam(rnn$parameters, lr = 0.001),
  verbose=FALSE,
  tau = 0.1
) {

  # Helper function to train on mini-batches:
  train_batches <- function() {
    rnn$train()
    train_loss <- c()
    coro::loop(for (b in train_dl) {
      l <- train_batch(rnn, b, loss, optim)
      train_loss <- c(train_loss, l)
    })
    return(mean(train_loss))
  }

  # Helper function to compute validation loss:
  compute_valid_loss <- function() {
    rnn$eval()
    valid_loss <- c()
    coro::loop(for (b in valid_dl) {
      l <- valid_batch(rnn, b, loss)
      valid_loss <- c(valid_loss, l)
    })
    return(mean(valid_loss))
  }

  valid_loss_previous <- compute_valid_loss()
  valid_loss_change_trailing <- Inf
  epoch <- 1

  while (epoch <= num_epochs && valid_loss_change_trailing > tau) {

    train_loss <- train_batches()
    if (verbose) {
      cat(sprintf("\nEpoch %d, training: loss: %3.5f \n", epoch, train_loss))
    }

    valid_loss <- compute_valid_loss()
    if (verbose) {
      cat(sprintf("\nEpoch %d, validation: loss: %3.5f \n", epoch, valid_loss))
    }

    valid_loss_change <- valid_loss_previous - valid_loss
    if (epoch == 1) {
      valid_loss_change_trailing <- valid_loss_change
    } else {
      valid_loss_change_trailing <- weighted.mean(
        c(valid_loss_change_trailing, valid_loss_change),
        c(1/epoch, 1 - (1/epoch))
      )
    }
    valid_loss_previous <- mean(valid_loss)
    epoch <- epoch + 1

  }
}




