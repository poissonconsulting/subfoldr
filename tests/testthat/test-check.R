context("check")

test_that("check_filename", {
  rm(list = ls())
  expect_identical(check_filename("x"), "x")
  expect_error(check_filename('"e'), '"e is not a valid file name')
  expect_error(check_filename("obj[i]"), "obj\\[i\\] is not a valid file name")
})
