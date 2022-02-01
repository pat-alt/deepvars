
prepare_deepvar_model <- function(deepvar_data, size_ensemble) {

  # Setup:
  list2env(getOption("deepvar.model"), envir = environment()) # load data options
  input_size <- deepvar_data$K
  output_size <- deepvar_data$n_ahead
  response <- deepvar_data$response
  device <- getOption("deepvar.device")

  model_list <- lapply(
    1:length(response),
    function(k) {
      # Build ensemble for response variable k:
      ensemble <- lapply(
        1:size_ensemble,
        function(rnn) {
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
          return(net)
        }
      )
      model <- list(
        ensemble = ensemble,
        train_dl = deepvar_data$train_dl[[k]],
        valid_dl = deepvar_data$valid_dl[[k]]
      )
      return(model)
    }
  )

  deepvar_model <- list(
    model_list = model_list,
    model_data = deepvar_data,
    size_ensemble = size_ensemble
  )
  class(deepvar_model) <- "deepvar_model"
  return(deepvar_model)
}


