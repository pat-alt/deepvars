
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
  if (is.null(batch_size)) {
    batch_size <- round(0.2 * N)
  }
  # Train/validation split:
  list2env(train_val_split(data, train_size = train_size), envir = environment())

  # Training data sets:
  train_dl <- lapply(
    response,
    function(response_var) {
      dvar_dataset(train_ds, response_var, lags, n_ahead, sample_frac = sample_frac) |>
        torch::dataloader(batch_size = batch_size)
    }
  )

  # Validation data sets:
  valid_dl <- lapply(
    response,
    function(response_var) {
      dvar_dataset(valid_ds, response_var, lags, n_ahead, sample_frac = sample_frac) |>
        torch::dataloader(batch_size = batch_size)
    }
  )

  # Output:
  dvar_data <- list(
    train_dl=train_dl,
    valid_dl=valid_dl,
    response=response,
    lags=lags,
    n_ahead=n_ahead,
    K=K,
    N=N,
    var_names=var_names,
    data=data,
    data_opts=getOption("deepvar.data")
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

