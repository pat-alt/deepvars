# Prepare data for RNN: ----
deepvar_dataset <- torch::dataset(
  name = "deepvar_dataset",

  initialize = function(X, response, lags, n_ahead, sample_frac = 1, train_mean, train_sd) {

    self$lags <- lags
    self$n_ahead <- n_ahead
    self$response <- response
    self$train_mean <- train_mean
    self$train_sd <- train_sd
    self$X <- torch::torch_tensor(t((t(X) - self$train_mean)/self$train_sd)) # of dimension (D x T)

    n <- dim(self$X)[1] - self$lags - self$n_ahead + 1

    if (n < 1) stop(sprintf(
      "Your validation set contains less observations (%i) than the sum of `lags` and `n_ahead` (%i). Adjust the train sample ratio.",
      dim(self$X)[1],
      self$lags + self$n_ahead
    ))

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

deepvar_input_data <- torch::dataset(
  name = "deepvar_input_data",

  initialize = function(X, lags, train_mean, train_sd, use_last=TRUE) {

    self$lags <- lags
    self$train_mean <- train_mean
    self$train_sd <- train_sd
    self$X <- torch::torch_tensor(t((t(X) - self$train_mean)/self$train_sd)) # of dimension (D x T)

    n <- dim(self$X)[1] - self$lags + use_last

    self$starts <- 1:n

  },

  .getitem = function(i) {

    start <- self$starts[i]
    end <- start + self$lags - 1

    list(
      X = self$X[start:end,]
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

  device <- torch::torch_device(getOption("deepvar.device"))

  optim$zero_grad() # in
  output <- rnn(b$X$to(device = device))
  target <- b$y$to(device = device)

  l <- loss(output, target)
  l$backward()
  optim$step()

  l$item()
}

valid_batch <- function(rnn, b, loss) {

  device <- torch::torch_device(getOption("deepvar.device"))

  output <- rnn(b$X$to(device = device))
  target <- b$y$to(device = device)

  l <- loss(output, target)
  l$item()

}

forward_rnn <- function(rnn, train_dl, valid_dl, loss, optim_fun, optim_args, num_epochs, verbose=FALSE, tau = 0.1, patience=10, show_progress=TRUE, checkpoint_path="checkpoint.pt") {

  device <- torch::torch_device(getOption("deepvar.device"))
  optim <- do.call(optim_fun, c(params = list(rnn$parameters), optim_args))
  checkpoint_path <- file.path(tempdir(),checkpoint_path) # create a temporary directory for checkpoints

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

  compute_train_loss <- function() {
    rnn$eval()
    train_loss <- c()
    coro::loop(for (b in train_dl) {
      l <- valid_batch(rnn, b, loss)
      train_loss <- c(train_loss, l)
    })
    return(mean(train_loss))
  }

  valid_loss_all <- compute_valid_loss()
  train_loss_all <- compute_train_loss()
  stop_early <- FALSE
  epoch <- 1

  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "  Training [:bar] :percent in :elapsed",
      total = num_epochs, clear = FALSE, width = 60
    )
  }

  best_valid <- valid_loss_all[1]
  best_train <- train_loss_all[1]
  best_epoch <- epoch

  while (epoch <= num_epochs & !stop_early) {

    if (show_progress) pb$tick()

    train_loss <- train_batches()
    if (verbose) {
      cat(sprintf("\nEpoch %d, training: loss: %3.5f \n", epoch, train_loss))
    }

    valid_loss <- compute_valid_loss()
    if (verbose) {
      cat(sprintf("\nEpoch %d, validation: loss: %3.5f \n", epoch, valid_loss))
    }

    valid_loss_all <- c(valid_loss_all, valid_loss)
    train_loss_all <- c(train_loss_all, train_loss)
    avg_loss_all <- rowMeans(cbind(valid_loss_all,train_loss_all))*abs(valid_loss_all-train_loss_all)
    if (tail(avg_loss_all,1) == min(avg_loss_all)) {
      # Temporarily save best model to disk:
      best_valid <- valid_loss
      best_train <- train_loss
      best_epoch <- epoch
      try({torch::torch_save(rnn, checkpoint_path)})
    }
    if (epoch > patience[2]) {
      stop_early <- sum(diff(valid_loss_all)[(epoch - patience[2] + 1):epoch] > tau) > patience[1] & best_epoch != epoch
    }

    if (stop_early) {
      message(
        sprintf(
          "\nStopping early in epoch %i as change in validation loss has been higher than %0.5f for %i out the past %i epochs.\nUsing model with lowest combination of training and validation loss.",
          epoch,
          tau,
          patience[1],
          patience[2]
        )
      )
    }

    epoch <- epoch + 1

  }

  message(
    sprintf(
      "Best combination of training loss of %0.5f and validation loss of %0.5f achieved in epoch %i. Using corresponding model.",
      best_train,
      best_valid,
      best_epoch
    )
  )

  try({
    rnn <- torch::torch_load(checkpoint_path, device=device) # load best specification (in terms of validation loss)
  })


  return(rnn)

}






