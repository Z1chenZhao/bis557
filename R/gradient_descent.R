#' @title gradient_descent
#' @description Compute a linear regression with gradient descent method.
#'
#' @param form A formula with the format of "Y ~ .".
#' @param dat A dataframe.
#' @param contrasts A list of contrasts.
#' @param factor The factor to adjust beta, set default as 0.0001.
#' @param m.iteration The maximum iteration times to allow the loop to exit, set default as 1e5
#' @param b.difference The difference between previous sum of residuals and new sum of residuals
#' to allow the loop to exit, set default as 1e-10.
#' @return A list of coefficients from an linear regression model with gradient descent.
#' @examples
#' \dontrun{
#' gradient_descent(Sepal.Length ~ ., iris, contrasts)
#' }
#'
#' @importFrom stats model.frame model.matrix
#' @export

gradient_descent <- function(form, dat, contrasts = NULL, factor = 0.0001, m.iteration = 1e5, b.difference = 1e-10){
  #Eliminating all the NAs within the dataset
  dat_no_na <- model.frame(form, dat)
  #Define the model matrix
  X <- model.matrix(form, dat, contrasts.arg = contrasts)
  #Get the name of the dependent variable
  y_name <- as.character(form)[2]
  #Define the dependent variable
  Y <- as.matrix(subset(dat_no_na, select = y_name), ncol = 1)
  #Define beta_k
  beta_k <- matrix(1, ncol = 1, nrow = ncol(X))
  #Use a function to find the sum of squared residuals
  res <- function(beta, X, Y) {
    drop(t(Y) %*% Y + t(beta) %*% t(X) %*% X %*% beta - 2 * t(Y) %*% X %*% beta)
  }
  #Use a function to calculate the gradient times the loss function
  gl <- function(beta, X, Y){
    (- 2 * t(X) %*% Y + 2 * t(X) %*% X %*% beta)
  }
  #Run the gradient descent model if the dataset has no collinarity problem
  if(qr(X)$rank == dim(X)[2]){
    #Give initiate values to the difference and iteration time
    diff = 1
    iter = 0
    #Compute difference between previous sum of residuals and new sum of residuals, use while loop
    #to update and exist when it is less than the given difference
    while ((iter < m.iteration) & (diff > b.difference)) {
      #Calculate previous sum of residuals
      res.1 <- res(beta_k, X, Y)
      #Get new beta
      beta_k <- beta_k - factor*gl(beta_k, X, Y)
      #Calculate new sum of residuals
      res.2 <- res(beta_k, X, Y)
      #Calculate the difference between two residuals
      diff <- abs(res.1 - res.2)
      #Update iteration time
      iter = iter + 1
    }
    #Return the coefficients of beta_k
    return(list(coefficients = beta_k))
  }
  #Run the linear model if the dataset has collinarity problem
  else{
    print("The dataset has collinarity problem, run linear model instead")
    linear_model(form, dat, contrasts = NULL)
  }
}


