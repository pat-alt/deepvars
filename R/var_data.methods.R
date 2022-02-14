# Invert scaling
#' @export
invert_scaling.var_data <- function(y, var_data, k=NULL) {
  if (!is.null(var_data$scaler)) {
    y <- data.table::data.table(y)
    if (!is.null(k)) {
      var_names <- var_data$var_names[k]
    } else {
      var_names <- var_data$var_names
    }
    colnames(y) <- var_names
    y[
      ,
      (var_names):=lapply(
        var_names,
        function(var) {
          get(var) * var_data$scaler$sd[1,get(var)] + var_data$scaler$means[1,get(var)]
        }
      )
      ]
  } else {
    message("Data was never scaled.")
  }
  return(y)
}

#' @export
invert_scaling <- function(y, var_data, k=NULL) {
  UseMethod("invert_scaling", var_data)
}

# Prepare for LSTM model
#' @export
var2deepvar.var_data <- function(var_data) {
  # Unpack:
  list2env(var_data, envir = environment())
  # Remove constant if necessary:
  if (ncol(X)!=ncol(y)) {
    X <- X[,-1]
  }
  # Reshape:
  X <- keras::array_reshape(X, c(nrow(X),1,ncol(X)))
  y <- keras::array_reshape(y, c(nrow(y),1,ncol(y)))

  # Output:
  deepvar_data <- list(
    X=X,
    y=y,
    lags=lags,
    K=K,
    var_names=var_names,
    scaler=scaler,
    data=data
  )
  class(deepvar_data) <- c("deepvar_data", "var_data")
  return(deepvar_data)
}

#' @export
var2deepvar <- function(var_data) {
  UseMethod("var2deepvar", var_data)
}


