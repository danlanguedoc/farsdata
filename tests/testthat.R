library(testthat)
library(farsdata)
library(dplyr)


test_that("make_filename function", expect_equal(make_filename(2015), "accident_2015.csv.bz2"))

setwd(system.file("extdata", package = "farsdata"))

test_that("fars_read() works correctly", {
  expect_is(fars_read("accident_2015.csv.bz2"), "tbl_df")
  expect_error(fars_read("accident_2016.csv.bz2"))
})
