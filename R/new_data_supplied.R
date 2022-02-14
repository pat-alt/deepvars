new_data_supplied <- function(X,y) {
  new_data_supplied <- !is.null(X) & !is.null(y)
  if ((!is.null(X) & is.null(y)) | (is.null(X) & !is.null(y))) {
    stop("Either X or y is NULL. Need both arguments to be specified to compute function for new data.")
  }
  return(new_data_supplied)
}
