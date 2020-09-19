
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis557

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/Z1chenZhao/bis557.svg?branch=master)](https://travis-ci.com/Z1chenZhao/bis557)
[![Codecov test
coverage](https://codecov.io/gh/Z1chenZhao/bis557/branch/master/graph/badge.svg)](https://codecov.io/gh/Z1chenZhao/bis557?branch=master)
<!-- badges: end -->

The goal of bis557 is to write R functions for homework assignments

## Installation

You can install the released version of bis557 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("bis557")
```

## Example

``` r
library(bis557)
## basic example code
data(iris)
linear_model(Sepal.Length ~ ., iris)
#> $coefficients
#> [1]  2.1712663  0.4958889  0.8292439 -0.3151552 -0.7235620 -1.0234978
```
