companion_form <- function(A, constant=TRUE) {
  K <- ncol(A)
  lags <- (nrow(A)-constant)/K
  if(constant==TRUE) {
    top <- t(A[-1,])
  } else {
    top <- t(A)
  }
  bottom <- cbind(
    diag(K*(lags-1)),
    matrix(rep(0,K**2 * (lags-1)),ncol = K)
  )
  A_comp <- rbind(top,bottom)
  return(A_comp)
}
