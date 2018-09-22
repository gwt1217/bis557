
#' Fit a linear model
#'
#' @description This function passes arguments to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats model.matrix 
#' @export 
linear_model <- function(formula, data) {
  a<- all.vars(formula)
  #create response varible y
  y <- data[,a[1]]
  #create matrix containing coefficients and intercept
  matrix <- model.matrix(formula, data)
  output <- list()
  output$coefficients <- qr.coef(qr(matrix),y)
  #create lm object
  class (output) = "lm"
  return(output)
}