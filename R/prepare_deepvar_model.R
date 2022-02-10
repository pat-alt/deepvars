#' Title
#'
#' @param deepvar_data
#'
#' @return
#' @export
#'
#' @examples
prepare_deepvar_model <- function(deepvar_data) {

  # Setup:
  list2env(getOption("deepvar.model"), envir = environment()) # load data options
  input_size <- deepvar_data$K
  output_size <- deepvar_data$n_ahead
  response <- deepvar_data$response
  device <- torch::torch_device(getOption("deepvar.device"))

  model_list <- lapply(
    1:length(response),
    function(k) {
      net <- RNN(
        type = type,
        input_size = input_size,
        hidden_size = hidden_size,
        linear_size = linear_size,
        output_size = output_size,
        num_layers = num_layers,
        dropout = dropout,
        linear_dropout = linear_dropout
      )
      net <- net$to(device = device)
      model <- list(
        net = net,
        train_dl = deepvar_data$train_dl[[k]],
        valid_dl = deepvar_data$valid_dl[[k]]
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


