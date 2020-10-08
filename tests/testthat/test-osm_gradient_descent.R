library(testthat)

context("Test the function osm_gradient_descent")

test_that("Your osm_gradient_descentfunction works in an easy case.", {

  data(iris)

  fit_ngd <- osm_gradient_descent(Sepal.Length ~ ., iris)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_ngd$coefficients, fit_lm$coefficients, 0.3)
})


test_that("Your osm_gradient_descent function works in a tough case.", {

  data(lm_patho)

  fit_ngd <- osm_gradient_descent(y ~., lm_patho)

  fit_lm <- lm(y ~., lm_patho)

  expect_equivalent(fit_ngd$coefficients, fit_lm$coefficients, 0.3)
})
