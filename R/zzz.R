.onLoad <- function(libname, pkgname) {
  device <- ifelse(torch::cuda_is_available(),'cuda','cpu')
  opts = c(
    "deepvar.verbose" = "FALSE",
    "deepvar.device" = "device",
    "deepvar.data" = "list(train_size=0.75, sample_frac=1.0, batch_size=0.25)",
    "deepvar.model" = "list(type='lstm', hidden_size=32, linear_size=128, num_layers=2, dropout=0.25, linear_dropout=0.25)",
    "deepvar.train" = "list(loss=torch::nnf_mse_loss,optim_fun=torch::optim_adam,optim_args=list(),tau=-1e-5,parallelize=FALSE,patience=c(10,15),show_progress=TRUE)"
  )
  for (i in setdiff(names(opts),names(options()))) {
    eval(parse(text = paste0("options(",i,"=",opts[i],")")))
  }
}
