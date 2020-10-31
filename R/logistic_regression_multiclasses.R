#' logistic_regression_multiclasses
#'
#' @description Implement a classification model generalizing
#' logistic regression to accommodate more than two classes.
#'
#' @param X The design matrix
#' @param Y Response variable
#' @param mu_fun A function to use eta get mu
#' @param maxit The maximum iteration times to allow the loop to exit, set default as 1e5
#' @param tol The difference between previous beta and new beta to allow
#' the loop to exit, set default as 1e-5.
#'
#' @details  The code is adapted from lecture notes.
#' @return A list of beta coefficients.
#' @importFrom utils tail
#' @export
#'

logistic_regression_multiclasses <- function(X,
                                             Y,
                                             mu_fun,
                                             maxit = 1e5,
                                             tol = 1e-5){

  beta <- matrix(rep(0, ncol(X), ncol = 1))
  beta_diff <- c()
  for (i in 1:length(levels(Y))) {
    Y.multi <- as.numeric(Y == levels(Y)[i])

    for (j in seq_len(maxit)) {
      beta_old <- beta
      eta <- X %*% beta
      mu <- mu_fun(eta)
      beta <- beta_old + t(X) %*% (Y.multi - matrix(mu, ncol = 1))/10000
      beta_diff <- c(beta_diff, sum( (beta - beta_old)^2 ))

      if (tail(beta_diff, 1) < tol) {
        break
      }
    }
  }

  return(list(coefficients = beta))
}
