prepare_var_data <- function(data, lags=1, constant=TRUE, standardize=FALSE) {

  # Set up: ----
  var_names <- colnames(data)[colnames(data)!="date"] # variable names excluding date
  data <- data.table::as.data.table(data) # turn into data.table
  if ("date" %in% names(data)) {
    if (data[,class(date)[1]]!="Date") {
      warning("Date indexing is only implemented for date of class Date. Using simple integer index instead.")
      data[,date:=1:.N]
    }
  } else {
    data[,date:=1:.N]
  }
  data_out <- data.table::copy(data) # save a copy of all data
  data <- data[,.SD,.SDcols=var_names] # keep only model variables
  N <- nrow(data)-lags
  K <- ncol(data)

  # Standardize:
  if (standardize) {
    scaler <- list(
      means = data[,lapply(.SD, mean),.SDcols=var_names],
      sd = data[,lapply(.SD, sd),.SDcols=var_names]
    )
    data[,(var_names):=lapply(.SD, function(i) {(i-mean(i))/sd(i)}),.SDcols=var_names]
  } else {
    scaler <- NULL
  }

  # Reshape:
  new_names <- c(sapply(1:lags, function(p) sprintf("%s_l%i", var_names, p)))
  data[
    ,
    (new_names) := sapply(
      1:lags,
      function(lag) {
        data.table::shift(.SD, lag)
      }
    )
  ]
  # Dependent variable:
  y = as.matrix(data[(lags+1):.N,1:K])
  # Explanatory variables:
  if (constant==T) {
    const = 1
    X = cbind("constant"=1,as.matrix(data[(lags+1):.N,(K+1):ncol(data)]))
  } else {
    const = 0
    X = as.matrix(data[(lags+1):.N,(K+1):ncol(data)])
  }

  # Output:
  var_data <- list(
    X=X,
    y=y,
    lags=lags,
    K=K,
    N=N,
    var_names=var_names,
    scaler=scaler,
    data=data_out
  )
  class(var_data) <- "var_data"

  return(var_data)
}
