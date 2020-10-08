#' @title optimization_lambda
#' @description Optimizing the ridge parameter lambda by cross validation
#'
#' @param form A formula with the format of "Y ~ .".
#' @param dat A dataframe.
#' @param folds The number of folds to cross validate
#' @param lambdas A list of the ridge penalty term lambda.
#' @param contrasts A list of contrasts.
#'
#' @return The ridge regression parameter lambda that minimizes mse.
#' @importFrom stats formula qnorm sd model.matrix predict
#' @importFrom doParallel registerDoParallel
#' @importFrom rsample vfold_cv testing training
#' @importFrom foreach foreach %do% %dopar%
#' @import dplyr foreach rsample
#' @export
#'

optimization_lambda <- function (form, dat, folds = 10, lambdas = seq(0, 1, 0.1), contrasts= NULL) {
  registerDoParallel(cores = 10)
  folds <- vfold_cv(dat, v = folds)
  #Find mse:
  mse <- function(x, x_hat){
    return(mean((x - x_hat)^2))
  }
  #Calculate mse for each lambda
  i <- NULL
  `.`<- NULL
  lam <- NULL
  mse.l <- foreach(lambda = lambdas, .combine = rbind) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c) %do% {
      mse(
        testing(folds$splits[[i]])[[as.character(form[2])]],
        predict(ridge_regression(form, training(folds$splits[[i]]),
                                 lambda = lambda, contrasts = contrasts),
                testing(folds$splits[[i]]))
      )
    }
  }
  #Create a tibble results
  mse.t <- tibble(mean = apply(mse.l, 1, mean), sd = apply(mse.l, 1, sd), lambda = lambdas) %>%
    mutate(upper = mean + qnorm(0.975) * sd / nrow(.), lower = mean - qnorm(0.975) * sd / nrow(.))
  #Find the lambda that minimizes mse
  lambda_min <- mse.t$lambda[which.min(mse.t$mean)]
  lambda_min
}
