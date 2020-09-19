#' @title linear_model
#' @description Compute a linear regression.
#'
#' @param form A formula with the format of "Y ~ ."
#' @param dat A dataframe.
#' @param contrasts A list of contrasts.
#' @return A list of coefficients from an linear regression model.
#' @examples
#' linear_model(Sepal.Length ~ ., iris, contrasts)
#'
#' @importFrom stats model.frame model.matrix
#' @export


#Define the linear model function with a formula, a data frame and a list of contrasts for factor variables as input
linear_model <- function(form, dat, contrasts = NULL) {
  #Eliminating all the NAs within the dataset
  dat_no_na <- model.frame(form, dat)
  #Define the model matrix
  X <- model.matrix(form, dat, contrasts.arg = contrasts)
  #Get the name of the dependent variable
  y_name <- as.character(form)[2]
  #Define the dependent variable
  Y <- as.matrix(subset(dat_no_na, select = y_name), ncol = 1)
  #Solve for the coefficients
  beta <- qr.solve(qr(X), Y)
  #Change beta to numeric format
  beta <- as.numeric(beta)
  #Replace all the zero term to NA
  beta[beta==0] <- NA
  #Add row names
  names(beta) <- rownames(beta)
  #Return the coefficients
  return(list(coefficients = beta))
}


