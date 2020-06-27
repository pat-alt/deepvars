#' compute_Theta
#'
#' @param Phi
#' @param B_0
#'
#' @return
#' @export
#'
#' @description Helper function used to compute Theta coefficients as in Kilian, Luetkepohl
compute_Theta = function(Phi, B_0) {


  # THESE ARE THE IRFS!!!! Rework IRF function
  Theta = lapply(1:length(Phi), function(i) {
    Phi[[i]] %*% solve(B_0)
  })

  return(Theta)

}
