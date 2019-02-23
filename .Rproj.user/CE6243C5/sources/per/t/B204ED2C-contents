#' fit a simple mediation model
#'
#' give the parameter estimates of a simple mediation model
#'
#' @param rxm input of the correlation between X and M
#' @param rxy input of the correlation between X and Y
#' @param rym input of the correlation between Y and M
#' @param nobs input of the number of observations in your sample
#'
#' @return product of the two input numbers
#' @import lavaan
#' @export
simple.mediation <- function(rxm=-2,rxy=-2,rym=-2,nobs=0) {
  #check input
  if (!(check_input(rxm) && check_input(rxy) && check_input(rym))) {
    return("Error: input of correlations is problematic!")
  }
  if (nobs<=0) {
    return("Error: input of sample size is problematic!")
  }
  #fit the model with sem package
  model.mediation <- '
  M ~ a * X
  Y ~ b * M + c * X
  ind := a * b
  '
  cor.matrix <- matrix(c(1,rxm,rxy,rxm,1,rym,rxy,rym,1),3,3)
  rownames(cor.matrix) <- colnames(cor.matrix) <- c("X","M","Y")
  fit <- sem(model.mediation,sample.cov=cor.matrix,sample.nobs=nobs)
  #get the parameter estimates
  result <- parameterEstimates(fit)
  return(result)
}

