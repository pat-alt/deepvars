#' @export
split <- function(data, ratio_train=0.8, n_train=NULL, end_training=NULL) {

  if (is.null(n_train) & is.null(end_training)) {
    n_train <- floor(ratio_train * nrow(data))
  } else if (is.null(n_train) & !is.null(end_training)) {
    n_train <- data[,which(date==end_training)]
  }
  train_data <- data[1:n_train,]
  test_data <- data[(n_train+1):.N,]
  train_test_split <- list(
    train_data = train_data,
    test_data = test_data,
    data = data,
    n_train = n_train
  )
  class(train_test_split) <- "train_test_split"

  return(train_test_split)
}

#' @export
prepare_test_data.train_test_split <- function(train_test_split, lags) {

  test_data <- train_test_split$data[(train_test_split$n_train+1-lags):.N,]
  X_test <- prepare_var_data(test_data, lags)$X
  y_test <- prepare_var_data(test_data, lags)$y
  return(
    list(
      X_test = X_test,
      y_test = y_test
    )
  )
}

#' @export
prepare_test_data <- function(train_test_split, lags) {
  UseMethod("prepare_test_data", train_test_split)
}
