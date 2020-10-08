#' @title  out-of-sample gradient descent
#' @name osm_gradient_descent
#' @description Implement gradient descent for ordinary least squares
#'
#' @param form A formula with the format of "Y ~ .".
#' @param dat A dataframe.
#' @param contrasts A list of contrasts.
#' @param factor The factor to adjust beta, set default as 0.0001.
#' @param m.iteration The maximum iteration times to allow the loop to exit, set default as 1e5
#' @param b.difference The difference between previous sum of residuals and new sum of residuals
#' to allow the loop to exit, set default as 1e-10.
#' @param F The number of folds, set default as 10
#' @return A list of coefficients from an linear regression model with gradient descent.
#' @examples
#' \dontrun{
#' osm_gradient_descent(Sepal.Length ~ ., iris, contrasts)
#' }
#'
#' @importFrom stats model.frame model.matrix
#' @export

osm_gradient_descent <- function(form, dat, contrasts = NULL, factor = 0.0001, m.iteration = 1e5, b.difference = 1e-10, F=10 ){
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
  #Use a function to calculate the gradient times the loss function
  gl <- function(beta, X, Y){
    (- 2 * t(X) %*% Y + 2 * t(X) %*% X %*% beta)
  }
  #Run the gradient descent model if the dataset has no collinarity problem
  if(qr(X)$rank == dim(X)[2]){
    #Give initiate values to the difference and iteration time
    diff = 1
    iter = 0
    set.seed(123)
    folds <- vfold_cv(dat, v = F)
    res.1 <- NULL
    for (i in 1:F){
      res.1 <- c(res.1,
                  as.vector(assessment(folds$splits[[i]])[,y_name]-(model.matrix(form, assessment(folds$splits[[i]])) %*% beta_k)))
    }
    mse <- mean(res.1^2)
    #Compute difference between previous sum of residuals and new sum of residuals, use while loop
    #to update and exist when it is less than the given difference
    while ((iter < m.iteration) & (diff > b.difference)) {
      #Get new beta
      beta_k.1 <- beta_k - factor*gl(beta_k, X, Y)
      res.2 <- NULL
      for (j in 1:F){
        res.2 <- c(res.2,
                   as.vector(assessment(folds$splits[[j]])[,y_name]-(model.matrix(form, assessment(folds$splits[[j]])) %*% beta_k.1)))
      }
      mse.new <- mean(res.2^2)
      #Calculate the difference between two residuals
      diff <- abs(mse.new - mse)
      #Update iteration time
      iter <- iter + 1
      mse <- mse.new
      beta_k <- beta_k.1
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


