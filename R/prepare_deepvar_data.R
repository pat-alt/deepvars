
prepare_deepvar_data <- function(train_ds, valid_ds, lags, n_ahead=1, response=NULL) {

  # Setup:
  list2env(getOption("deepvar.data"), envir = environment()) # load data options
  var_names <- colnames(train_ds)
  K <- ncol(train_ds)
  N <- nrow(train_ds)
  # Response variables:
  if (is.null(response)) {
    response <- 1:K
  }
  # Batch size:
  if (batch_size < 1) {
    batch_size <- round(batch_size * N)
  }

  # Normalizing features:
  train_mean <- colMeans(train_ds)
  train_sd <- sapply(1:ncol(train_ds), function(i) sd(train_ds[,i]))

  # Training data sets:
  train_dl <- lapply(
    response,
    function(response_var) {
      deepvar_dataset(train_ds, response_var, lags, n_ahead, sample_frac = sample_frac, train_mean, train_sd) |>
        torch::dataloader(batch_size = batch_size)
    }
  )

  # Validation data sets:
  valid_dl <- lapply(
    response,
    function(response_var) {
      deepvar_dataset(valid_ds, response_var, lags, n_ahead, sample_frac = sample_frac, train_mean, train_sd) |>
        torch::dataloader(batch_size = 1)
    }
  )

  # Full training data set as input (for fitted values etc.):
  full_dl <- deepvar_input_data(as.matrix(train_ds), lags = lags, train_mean = train_mean, train_sd = train_sd, use_last=FALSE) |>
    torch::dataloader(batch_size = N)

  # Output:
  deepvar_data <- list(
    train_dl=train_dl,
    valid_dl=valid_dl,
    full_dl=full_dl,
    response=response,
    lags=lags,
    n_ahead=n_ahead,
    K=K,
    N=N,
    var_names=var_names,
    data=train_ds,
    data_opts=getOption("deepvar.data"),
    train_mean=train_mean,
    train_sd=train_sd,
    var_names=var_names,
    response_var_names=var_names[response]
  )
  class(deepvar_data) <- "deepvar_data"

  return(deepvar_data)
}

# train_val_split <- function(data, train_size) {
#   N <- nrow(data)
#   end_train <- round(train_size * N)
#   data_train <- data[1:end_train,] |> as.matrix()
#   data_val <- data[(end_train+1):N,] |> as.matrix()
#   return(list(train_ds=data_train, valid_ds=data_val))
# }

