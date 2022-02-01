.onLoad <- function(libname, pkgname) {
  opts = c(
    "deepvar.verbose" = "FALSE",
    "deepvar.device" = "torch::torch_device(if (torch::cuda_is_available()) 'cuda' else 'cpu')",
    "deepvar.data" = "list(train_size=0.75, sample_frac=1.0, batch_size=0.25)",
    "deepvar.model" = "list(type='lstm', hidden_size=32, linear_size=128, num_layers=2, dropout=0.25, linear_dropout=0.25)",
    "deepvar.train" = "list(loss=torch::nnf_mse_loss,optim_fun=torch::optim_adam,optim_args=list(),tau=0.1,parallelize=FALSE)"
  )
  for (i in setdiff(names(opts),names(options()))) {
    eval(parse(text = paste0("options(",i,"=",opts[i],")")))
  }
}
