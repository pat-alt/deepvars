#' fevd
#'
#' @param varresult
#' @param n.ahead
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @return
#' @export
#'
#' @description Function used to compute forecast error variance decomposition as in Kilian, Luetkepohl
#'

fevd = function(varresult,
                n.ahead=10,
                plot=T,
                ylab="Variance contribution",
                xlab="Forecast horizon",
                title=NULL,
                theme=ggplot2::theme_bw())

{

  # Get outputs ----
  data = data.table::as.data.table(varresult$data)
  constant = varresult$constant # boolean
  const = ifelse(constant, 1, 0)
  lag = varresult$lag
  A = varresult$A
  A_comp = varresult$A_comp
  K = varresult$K
  df = varresult$df
  Sigma_res = varresult$Sigma_res
  var_names = varresult$var_names

  # Compute MSPE ----
  Phi = red_ir(lag, K, A_comp, const, n.ahead)
  B_0 = identify_chol(Sigma_res)
  Theta = compute_Theta(Phi, B_0)
  theta_kj_sq = compute_theta_kj_sq(Theta, n.ahead)
  mspe = compute_mspe(Theta, n.ahead)

  # Compute percentage contributions ----
  fevd_list = lapply(1:K, function(k) {
    t(sapply(1:length(mspe), function(h) {
      mspe_k = mspe[[h]][k,k]
      theta_k_sq = theta_kj_sq[[h]][,k]
      fevd = theta_k_sq/mspe_k
    }))
  })
  names(fevd_list) = var_names

  # Tidy ----
  fevd_tidy = data.table::rbindlist(
    lapply(1:length(fevd_list), function(k) {
      fevd_k = data.table::melt(data.table::data.table(fevd_list[[k]])[,h:=.I], id.vars = "h", variable.name = "j")
      fevd_k[,k:=names(fevd_list)[k]]
      data.table::setcolorder(fevd_k, c("k", "j", "h"))
    })
  )

  # Plot ----
  if (plot==T) {
    p = ggplot2::ggplot(data=fevd_tidy) +
      ggplot2::geom_area(ggplot2::aes(x=h, y=value, fill=j)) +
      ggplot2::facet_wrap(k ~ .) +
      ggplot2::scale_x_continuous(
        expand=c(0,0)
      ) +
      ggplot2::scale_fill_discrete(
        name = "Shock"
      ) +
      ggplot2::labs(
        x =xlab,
        y=ylab,
        title = title
      ) +
      theme
    # print(p)
  }

  return(
    list(
      fevd_tidy=fevd_tidy,
      plot = p
    )
  )

}
