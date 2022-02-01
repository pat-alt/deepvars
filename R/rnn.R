# Prepare data for RNN: ----
dvar_dataset <- torch::dataset(
  name = "dvar_dataset",

  initialize = function(X, response, lags, n_ahead, sample_frac = 1, train_mean, train_sd) {

    self$lags <- lags
    self$n_ahead <- n_ahead
    self$response <- response
    self$train_mean <- train_mean
    self$train_sd <- train_sd
    self$X <- torch::torch_tensor(t((t(X) - self$train_mean)/self$train_sd)) # of dimension (D x T)

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

# Prepare the RNN: ----
RNN <- torch::nn_module(

  initialize = function(
    type="lstm",
    input_size, hidden_size, linear_size, output_size, num_layers = 2,
    dropout = 0.25, linear_dropout = 0.25
  ) {

    self$type <- type
    self$num_layers <- num_layers
    self$linear_dropout <- linear_dropout
    self$output_size <- output_size

    self$rnn <- if (self$type == "gru") {
      torch::nn_gru(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        dropout = dropout,
        batch_first = TRUE
      )
    } else {
      torch::nn_lstm(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        dropout = dropout,
        batch_first = TRUE
      )
    }

    # An MLP for multi-step ahead predictions:
    self$mlp <- torch::nn_sequential(
      torch::nn_linear(hidden_size, linear_size),
      torch::nn_relu(),
      torch::nn_dropout(linear_dropout),
      torch::nn_linear(linear_size, output_size)
    )

    # A simple output layer:
    self$output <- torch::nn_linear(hidden_size, 1)

  },

  forward = function(x) {

    x <- self$rnn(x)[[1]][,-1,]
    if (self$output_size > 1) {
      # If multi-step ahead, send final time step through MLP:
      x |> self$mlp()
    } else {
      # Else just return output:
      x |> self$output()
    }

  }

)

# Training: ----
train_batch <- function(rnn, b, loss, optim) {

  device <- getOption("deepvar.device")

  optim$zero_grad() # in
  output <- rnn(b$X$to(device = device))
  target <- b$y$to(device = device)

  l <- loss(output, target)
  l$backward()
  optim$step()

  l$item()
}

valid_batch <- function(rnn, b, loss) {

  device <- getOption("deepvar.device")

  output <- rnn(b$X$to(device = device))
  target <- b$y$to(device = device)

  l <- loss(output, target)
  l$item()

}

forward_rnn <- function(rnn, train_dl, valid_dl, loss, optim_fun, optim_args, num_epochs, verbose=FALSE, tau = 0.1) {

  device <- getOption("deepvar.device")
  optim <- do.call(optim_fun, c(params = list(rnn$parameters), optim_args))

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

  while (epoch <= num_epochs) {

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






