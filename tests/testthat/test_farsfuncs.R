## Performs a simple test to check the output from the make_filename function
library(testthat)
expect_that(farsfuncs::make_filename(2013),equals("accident_2013.csv.bz2"))

f13path<-system.file("..", "..", "inst", "extdata", "accident_2013.csv.bz2")
file.copy(from=c(f13path),to=getwd())
b <- fars_read('../../inst/extdata/accident_2013.csv.bz2')
expect_that(nrow(b), equals(30202))
