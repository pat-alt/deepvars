#' compute_mspe
#'
#' @param Theta
#' @param n.ahead
#'
#' @return
#' @export
compute_mspe = function(Theta,
                        n.ahead=10)

{
  mspe = lapply(1:n.ahead, function(h) {
    terms_to_sum = lapply(1:h, function(i) {
      tcrossprod(Theta[[i]])
    })
    mspe_h = Reduce(`+`, terms_to_sum)
  })
}
