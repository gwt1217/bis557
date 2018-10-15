#' Fit a ridge regression model
#'
#' @description This function passes parameters to the ridge regression function.
#' @param formula a formula
#' @param data a data.frame
#' @param lambda a tuning parameter
#' @return An ridge object
#' @importFrom stats model.matrix
#' @export

ridge_reg <- function(formula,lambda,data){
  m <- model.matrix(formula,data)
  y <- matrix(data[,as.character(formula)[2]], ncol=1)
  y <- y[as.numeric(rownames(m)),,drop=FALSE]
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  D <- diag(svals/(svals^2 + lambda))
  beta <- V %*% D %*% t(U) %*% y
  ret <- list(coefficients = beta)
  class (ret) <- "ridge_reg"
  ret
}


