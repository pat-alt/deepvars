prepare_var_data <- function(data, lags=1, constant=TRUE) {

  # Set up: ----
  var_names <- colnames(data) # variable names excluding date
  data_out <- data.table::as.data.table(data) # turn into data.table
  N <- nrow(data)-lags
  K <- ncol(data)

  # Reshape:
  y <- as.matrix(data[(lags+1):(N+lags),1:K])
  X <- embed(as.matrix(data), lags+1)[,-c(1:K)]
  colnames(X) <- c(sapply(1:lags, function(p) sprintf("%s_l%i", var_names, p)))
  if (constant) {
    X <- cbind("constant"=1,X)
  }

  # Output:
  var_data <- list(
    X=X,
    y=y,
    lags=lags,
    K=K,
    N=N,
    data=data_out,
    constant=constant,
    var_names=var_names,
    response_var_names=var_names
  )
  class(var_data) <- "var_data"

  return(var_data)
}
