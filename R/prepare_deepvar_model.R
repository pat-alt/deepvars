# Combined model: ----
#' Deep VAR model setup
#'
#' @param deepvar_data
#' @param n_units
#'
#' @return
#' @export
#'
#' @importFrom keras `%>%`
#'
#' @author Patrick Altmeyer
prepare_deepvar_model <- function(
  deepvar_data,
  num_units=50,
  num_layers=2,
  p_drop_out=0.5,
  epsilon=0.001,
  optimizer=keras::optimizer_adam(learning_rate=epsilon)
) {

  K <- deepvar_data$K
  N <- deepvar_data$N
  dim_input <- dim(deepvar_data$X)[2:3]
  model_list <- lapply(
    1:K,
    function(k) {

      # Build model:
      list_of_layers <- lapply(
        1:num_layers,
        function(layer) {
          list(
            keras::layer_lstm(
              units = num_units,
              return_sequences = ifelse(layer<num_layers, TRUE, FALSE),
              input_shape = dim_input
            ),
            keras::layer_dropout(rate = p_drop_out)
          )
        }
      )
      list_of_layers <- do.call(c, list_of_layers)
      model <- keras::keras_model_sequential(list_of_layers) %>%
        keras::layer_dense(units = 2, activation = "linear") %>%
        tfprobability::layer_distribution_lambda(
          function(x) {
            tfprobability::tfd_normal(
              loc = x[, 1, drop = FALSE], # mean
              scale = 1e-3 + tensorflow::tf$math$softplus(x[, 2, drop = FALSE]) # standard deviation
            )
          }
        )

      # Loss:
      negloglik <- function(y, model) - (model %>% tfprobability::tfd_log_prob(y))

      # Compile model:
      model %>%
        keras::compile(
          loss = negloglik,
          optimizer = optimizer
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
