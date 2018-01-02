library(testthat)
library(FARS)

test_that("make_filename function", expect_equal(make_filename(2015), "accident_2015.csv.bz2"))
