## code to prepare `DATASET` dataset goes here


usethis::use_data(DATASET, overwrite = TRUE)
#Read the dataset from local file
setwd("~/Desktop/Yale/BIS 557/HW/HW1/homework-1")
#Read the dataset
lm_patho <- read.csv("lm_patho.csv", header = T)
#Add the dataset to the package
setwd("~/Desktop/Yale/BIS 557/bis 557/bis557")
#Add the dataset
use_data(lm_patho)
