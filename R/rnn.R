# # Adapted from https://blogs.rstudio.com/ai/posts/2021-03-10-forecasting-time-series-with-torch_1/:
# model <- nn_module(
#
#   initialize = function(type="lstm", input_size, hidden_size=120, num_layers = 2, dropout = 0.25) {
#
#     self$type <- type
#     self$num_layers <- num_layers
#
#     self$rnn <- if (self$type == "gru") {
#       nn_gru(
#         input_size = input_size,
#         hidden_size = hidden_size,
#         num_layers = num_layers,
#         dropout = dropout,
#         batch_first = TRUE
#       )
#     } else {
#       nn_lstm(
#         input_size = input_size,
#         hidden_size = hidden_size,
#         num_layers = num_layers,
#         dropout = dropout,
#         batch_first = TRUE
#       )
#     }
#
#     self$output <- nn_linear(hidden_size, 1)
#
#   },
#
#   forward = function(x) {
#
#     # list of [output, hidden]
#     # we use the output, which is of size (batch_size, n_timesteps, hidden_size)
#     x <- self$rnn(x)[[1]]
#
#     # from the output, we only want the final timestep
#     # shape now is (batch_size, hidden_size)
#     x <- x[ , dim(x)[2], ]
#
#     # feed this to a single output neuron
#     # final shape then is (batch_size, 1)
#     x %>% self$output()
#   }
#
# )
