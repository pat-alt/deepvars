
prepare_deepvar_data <- function(data, lags, n_ahead=1, response=NULL) {

  # Setup:
  list2env(getOption("deepvar.data"), envir = environment()) # load data options
  var_names <- colnames(data)
  K <- ncol(data)
  N <- nrow(data)
  # Response variables:
  if (is.null(response)) {
    response <- 1:K
  }
  # Batch size:
  if (batch_size < 1) {
    batch_size <- round(batch_size * N)
  }
  # Train/validation split:
  list2env(train_val_split(data, train_size = train_size), envir = environment())

  # Normalizing features:
  train_mean <- colMeans(train_ds)
  train_sd <- sapply(1:ncol(train_ds), function(i) sd(train_ds[,i]))

  # Training data sets:
  train_dl <- lapply(
    response,
    function(response_var) {
      dvar_dataset(train_ds, response_var, lags, n_ahead, sample_frac = sample_frac, train_mean, train_sd) |>
        torch::dataloader(batch_size = batch_size)
    }
  )

  # Validation data sets:
  valid_dl <- lapply(
    response,
    function(response_var) {
      dvar_dataset(valid_ds, response_var, lags, n_ahead, sample_frac = sample_frac, train_mean, train_sd) |>
        torch::dataloader(batch_size = 1)
    }
  )

  # Whole data sets:
  full_dl <- lapply(
    response,
    function(response_var) {
      dvar_dataset(as.matrix(data), response_var, lags, n_ahead, sample_frac = 1.0, train_mean, train_sd) |>
        torch::dataloader(batch_size = N)
    }
  )

  # Output:
  dvar_data <- list(
    train_dl=train_dl,
    valid_dl=valid_dl,
    full_dl=full_dl,
    response=response,
    lags=lags,
    n_ahead=n_ahead,
    K=K,
    N=N,
    var_names=var_names,
    data=data,
    data_opts=getOption("deepvar.data"),
    train_mean=train_mean,
    train_sd=train_sd
  )
  class(dvar_data) <- "dvar_data"

  return(dvar_data)
}

train_val_split <- function(data, train_size) {
  N <- nrow(data)
  end_train <- round(train_size * N)
  data_train <- data[1:end_train,] |> as.matrix()
  data_val <- data[(end_train+1):N,] |> as.matrix()
  return(list(train_ds=data_train, valid_ds=data_val))
}

