#' VAR_stable
#'
#' @param varresult Output from VAR estimation
#' @param verbose Logical: if TRUE, test result will be printed to console.
#'
#' @return
#' @export
#'
#'
#' @author Patrick Altmeyer

VAR_stable = function(varresult, verbose=T) {

  # Get outputs ----
  A_comp = varresult$A_comp # coefficient matrix in companion form
  K = varresult$K # number of variables in the system
  lag = varresult$lag

  # Perform test ----
  eigen_decomp = eigen(A_comp)
  eigenvals = eigen_decomp$values
  test_result = ifelse(max(abs(eigenvals)) < 1,"The VAR is stable.","The VAR is not stable")
  if (verbose==T) {
    message(test_result)
  }
  test_summary = data.table::data.table(lambda=eigenvals,in_unit_circle=T)
  test_summary[,in_unit_circle:=abs(eigenvals)<1]

  return(
    list(
      eigenvals = eigenvals,
      test_result = test_result
      # test_summary = test_summary
    )
  )

}
