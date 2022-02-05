#' @export
train_val_test_split <- function(data, train_size=0.8) {
  N <- nrow(data)
  end_train <- round(train_size * N)
  end_val <- end_train + round((N - end_train)/2)
  data_train <- data[1:end_train,] |> as.matrix()
  data_val <- data[(end_train+1):end_val,] |> as.matrix()
  data_test <- data[(end_val+1):N,] |> as.matrix()

  train_val_test_split <- list(train=data_train, val=data_val, test=data_test)

  class(train_val_test_split) <- "train_val_test_split"

  return(train_val_test_split)
}


#' @export
prepare_test_data.train_val_test_split <- function(train_val_test_split, lags) {

  valid_data <- train_val_test_split$valid
  X_valid <- prepare_var_data(valid_data, lags)$X
  y_valid <- prepare_var_data(valid_data, lags)$y

  test_data <- train_val_test_split$test
  X_test <- prepare_var_data(test_data, lags)$X
  y_test <- prepare_var_data(test_data, lags)$y

  return(
    list(
      X_valid = X_valid,
      y_valid = y_valid,
      X_test = X_test,
      y_test = y_test
    )
  )
}

#' @export
prepare_test_data <- function(train_test_split, lags) {
  UseMethod("prepare_test_data", train_test_split)
}

#' #' @export
#' split_sample <- function(data, ratio_train=0.8, n_train=NULL, end_training=NULL) {
#'
#'   if (is.null(n_train) & is.null(end_training)) {
#'     n_train <- floor(ratio_train * nrow(data))
#'   } else if (is.null(n_train) & !is.null(end_training)) {
#'     n_train <- data[,which(date==end_training)]
#'   }
#'   train_data <- data[1:n_train,]
#'   test_data <- data[(n_train+1):.N,]
#'   train_test_split <- list(
#'     train_data = train_data,
#'     test_data = test_data,
#'     data = data,
#'     n_train = n_train
#'   )
#'   class(train_test_split) <- "train_test_split"
#'
#'   return(train_test_split)
#' }
#'
#'
