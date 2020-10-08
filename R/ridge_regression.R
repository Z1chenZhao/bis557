#' @title ridge_regression
#' @description Compute a linear regression with gradient descent method.
#'
#' @param form A formula with the format of "Y ~ .".
#' @param dat A dataframe.
#' @param lambda The ridge penalty term lambda set default as 0.
#' @param contrasts A list of contrasts.
#'
#' @return A list of coefficients.
#' @importFrom stats model.frame model.matrix
#' @export
#'

ridge_regression <- function(form, dat, lambda=0, contrasts=NULL) {
  #Eliminating all the NAs within the dataset
  dat_no_na <- model.frame(form, dat)
  rownames(data) <- NULL
  #Define the model matrix
  X <- model.matrix(form, dat_no_na, contrasts.arg = contrasts)
  #Get the name of the dependent variable
  y_name <- as.character(form)[2]
  #Define the dependent variable
  Y <- as.matrix(subset(dat_no_na, select = y_name), ncol = 1)
  #Center the dependent variable
  mean.Y <- mean(Y)
  Y <- Y - mean.Y
  #Center the response variables
  mean.X <- colMeans(X[, -1])
  X <- X[, -1] - rep(mean.X, rep(nrow(X), ncol(X) - 1))
  #Scale the response variables
  scale.X <- drop(rep(1/nrow(X), nrow(X)) %*% X^2)^0.5
  X <- X/rep(scale.X, rep(nrow(X), ncol(X)))
  #Set up beta matrix
  beta <- matrix(NA_real_, nrow = length(lambda), ncol = ncol(X))
  #Find the coefficients
  svd <- svd(X)
  beta <- svd$v %*% diag(svd$d/(svd$d^2 + lambda)) %*% t(svd$u) %*% Y
  #Return the scale
  beta <- t(as.matrix(beta/scale.X))
  #Calculate the intercept
  intercept <- mean.Y - beta %*% mean.X
  #Add it to the vector
  beta <- cbind(intercept, beta)
  beta <- as.vector(beta)
  #Name intercept and beta
  names(beta) <- c("Intercept", colnames(X))
  #Generate output
  beta
}

#' Prediction function for Ridge Regression
#'
#' @param object ridge_regression object
#' @param ... a dataframe
#'
#' @return An estimate of new Y given X.
#' @export
#'
predict.ridge_regression <- function(object, ...) {
  # extract dots
  dots <- list(...)
  x_frame <- dots[[1]]
  # check for bad arg
  if (!is.data.frame(x_frame)) {
    stop("The first argument should be a data.frame of values",
         "to predict")
  }
  # create new model matrix and predict
  X <- model.matrix(attributes(object)$formula, x_frame)
  X %*% object
}


