#' GLM_gradient_adaptive
#'
#' @description Implement a first-order solution for the GLM maximum likelihood
#' problem using only gradient information, avoiding the Hessian matrix.
#' Standard adaptive update Momentum.
#'
#' @param X The design matrix
#' @param Y Response variable
#' @param mu_fun A function to use eta get mu
#' @param mom The momentum update size, set default as 0.2
#' @param maxit The maximum iteration times to allow the loop to exit, set default as 1e5
#' @param tol The difference between previous beta and new beta to allow
#' the loop to exit, set default as 1e-5.
#'
#' @details  The code is adapted from lecture notes.
#' @return A list of beta coefficients.
#' @importFrom utils tail
#' @export
#'

GLM_gradient_adaptive <- function(X,
                                  Y,
                                  mu_fun,
                                  mom = 0.2,
                                  maxit = 1e5,
                                  tol = 1e-5){

  beta <- matrix(rep(0, ncol(X), ncol = 1))
  beta_diff <- c()
  for (i in seq_len(maxit)) {
    beta_old <- beta

    eta <- X %*% beta
    mu <- mu_fun(eta)
    beta <- beta_old + (t(X) %*% (Y - matrix(mu, ncol = 1))/10000) * mom^(1:i-1)
    beta_diff <- c(beta_diff, sum( (beta - beta_old)^2 ))

    if (tail(beta_diff, 1) < tol) {
      break
    }
  }
  return(list(coefficients = beta))
}
