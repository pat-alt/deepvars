#' Var_predict
#'
#' @param varresult
#' @param n.ahead
#' @param ci
#' @param ci_colour
#' @param plot
#' @param dates
#' @param plot_vars
#' @param linetype_fcast
#' @param ylab
#' @param xlab
#' @param title
#' @param theme
#'
#' @importFrom data.table .I .N .SD :=
#'
#' @return
#' @export
#'
VAR_predict = function(varresult,
                       n.ahead=10,
                       ci=c(.95, .75, .5),
                       ci_colour="darkblue",
                       plot=F,
                       dates=NULL,
                       plot_vars="all",
                       linetype_fcast="dashed",
                       ylab="Values",
                       xlab="Index",
                       title=NULL,
                       theme=ggplot2::theme_bw())
{

  # Get outputs ----
  data = data.table::as.data.table(varresult$data)
  constant = varresult$constant # boolean
  const = ifelse(constant, 1, 0)
  lag = varresult$lag
  se = varresult$se
  A = varresult$A
  A_comp = varresult$A_comp
  K = varresult$K
  df = varresult$df
  Sigma_res = varresult$Sigma_res
  if (plot_vars=="all") {
    plot_vars=colnames(data)
  }

  # Pre-process ----
  fcst_tidy = data.table::melt(data, measure.vars = colnames(data))
  fcst_tidy[,idx:=1:.N,by=variable]
  fcst_tidy[,fcast:=0]
  ci_cols = lapply(1:length(ci), function(i) {
    sprintf("%s_%0.2f",
            c("lb", "ub"),
            ci[i])
  })
  fcst_tidy[,(unlist(ci_cols)):=0]
  fcst_tidy[idx==max(idx),fcast:=1,by=.(variable)]
  fcst_tidy[idx==max(idx),(unlist(ci_cols)):=value,by=.(variable)]

  # Compute Phi as in Kilian, Luetkepohl
  Phi = red_ir(lag, K, A_comp, const, n.ahead)
  B_0 = identify_chol(Sigma_res)
  Theta = compute_Theta(Phi, B_0)
  mspe = compute_mspe(Theta, n.ahead)

  # Forward iteration ----
  count = 0
  while (count<n.ahead) {
    count = count+1
    idx = fcst_tidy[,max(idx) + 1]
    # Explanatory variables:
    if (constant==T) {
      const = 1
      Z = cbind("constant"=1,as.matrix(data[,sapply(0:(lag-1), function(p) data.table::shift(.SD, p))][.N,]))
    } else {
      const = 0
      Z = as.matrix(data[,sapply(0:(lag-1), function(p) data.table::shift(.SD, p))][.N,])
    }
    # Prediction ----
    fcast = t(Z %*% A)
    data = rbind(data, t(fcast)) # update data

    # Confidence interval ----
    ci_list = lapply(1:length(ci), function(i) {

      p = (1 - ci[i])/2
      q = abs(stats::qnorm(p))
      state_vcov = mspe[[count]]
      ub = t(fcast + q * sqrt(diag(state_vcov)))
      lb = t(fcast - q * sqrt(diag(state_vcov)))
      conf_interval = data.table::data.table()
      conf_interval[,(ci_cols[[i]]) := .(c(lb),c(ub))]
      return(conf_interval)

    })
    conf_interval = Reduce(cbind, ci_list)

    # Tidy ----
    update_tidy = data.table::data.table(variable=colnames(A),
                             value=c(fcast),
                             conf_interval,
                             idx=idx,
                             fcast=1)
    fcst_tidy = merge(fcst_tidy, update_tidy, all=T)
    fcst_tidy = fcst_tidy[order(idx, variable)]
    data.table::setcolorder(fcst_tidy, c("idx", "variable"))

  }

  # Polygon dataset ----
  data_poly = data.table::melt(fcst_tidy[fcast==1][,(c("fcast", "value")):=NULL],
                   measure.vars = unlist(ci_cols), variable.name = "bound", value.name = "y")
  data_poly[,c("bound", "conf_level") := data.table::tstrsplit(bound, "_", fixed=TRUE)]
  data_poly[bound=="lb",
            (c("y", "idx")) := .(y=rev(y), idx=rev(idx)),
            by=.(variable, conf_level)]
  data_poly[,conf_level:=factor(conf_level, levels = sort(unique(conf_level), decreasing = T))]

  if (plot == T) {
    # Chart:
    ci_colour_vals = sapply(ci, function(i) scales::alpha(ci_colour, sqrt(1-i)))
    p = ggplot2::ggplot() +
      ggplot2::geom_line(data=fcst_tidy[variable %in% plot_vars], ggplot2::aes(x=idx, y=value)) +
      ggplot2::scale_x_continuous(
        expand=c(0,0)
      ) +
      ggplot2::geom_polygon(data=data_poly[variable %in% plot_vars],
                   ggplot2::aes(x = idx, y = y, fill = conf_level, group=conf_level)) +
      ggplot2::scale_fill_manual(
        name="Confidence\ninterval:",
        labels=function(x) {
          sprintf("%s%%",x)
        },
        values=ci_colour_vals
      ) +
      ggplot2::geom_line(data=fcst_tidy[fcast==1 & variable %in% plot_vars],
                ggplot2::aes(x=idx, y=value),
                linetype=linetype_fcast) +
      ggplot2::scale_linetype(guide="none") +
      ggplot2::facet_wrap(.~variable, scales="free", ncol = 1) +
      ggplot2::labs(
        x =xlab,
        y=ylab,
        title = title
      ) +
      theme
    print(p)
  } else {
    p = NULL
  }

  return(
    list(
      fcast_tidy = fcst_tidy,
      plot = p
    )
  )

}

