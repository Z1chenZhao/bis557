% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gradient_descent.R
\name{gradient_descent}
\alias{gradient_descent}
\title{gradient_descent}
\usage{
gradient_descent(
  form,
  dat,
  contrasts = NULL,
  factor = 1e-04,
  m.iteration = 1e+05,
  b.difference = 1e-10
)
}
\arguments{
\item{form}{A formula with the format of "Y ~ .".}

\item{dat}{A dataframe.}

\item{contrasts}{A list of contrasts.}

\item{factor}{The factor to adjust beta, set default as 0.0001.}

\item{m.iteration}{The maximum iteration times to allow the loop to exit, set default as 1e5}

\item{b.difference}{The difference between previous sum of residuals and new sum of residuals
to allow the loop to exit, set default as 1e-10.}
}
\value{
A list of coefficients from an linear regression model with gradient descent.
}
\description{
Compute a linear regression with gradient descent method.
}
\examples{
\dontrun{
gradient_descent(Sepal.Length ~ ., iris, contrasts)
}

}
