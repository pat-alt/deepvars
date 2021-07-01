apply_scaler_from_training <- function(X, scaler, lags, K) {
  var_indices <- rep.int(1:K,lags)
  X <- sapply(
    1:ncol(X),
    function(j) {
      if(scaler$fun == "normalize") {
        (X[,j]-unlist(scaler$means)[var_indices[j]])/unlist(scaler$sd)[var_indices[j]]
      }
    }
  )
  return(X)
}
