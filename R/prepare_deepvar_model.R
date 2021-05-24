#' Deep VAR model setup
#'
#' @param deepvar_data
#' @param n_units
#'
#' @return
#' @export
#'
#' @author Patrick Altmeyer
prepare_deepvar_model <- function(deepvar_data, n_units=50) {
  K <- deepvar_data$K
  dim_input <- dim(deepvar_data$X)[2:3]
  model_list <- lapply(
    1:K,
    function(k) {
      model <- keras::keras_model_sequential() %>%
        keras::layer_lstm(units = n_units, input_shape = dim_input) %>%
        keras::layer_dropout(0.5) %>%
        keras::layer_dense(units = 1)
      model %>%
        keras::compile(
          loss = "mae",
          optimizer = "adam"
        )
      return(model)
    }
  )

  deepvar_model <- list(
    model_list = model_list,
    model_data = deepvar_data
  )
  class(deepvar_model) <- "deepvar_model"
  return(deepvar_model)
}
