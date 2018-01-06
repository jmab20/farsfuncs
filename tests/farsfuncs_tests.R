## Performs a simple test to check the output from the make_filename function
library(testthat)
expect_that(farsfuncs::make_filename(2013),equals("accident_2013.csv.bz2"))
