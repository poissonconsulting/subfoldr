context("data")

test_that("data", {
  rm(list = ls())
  expect_identical(ls(), character(0))

  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("first", "2nd", "third")

  load_datas(main = main, sub = sub)
  expect_identical(sort(ls()), sort(c("main", "mtcars", "sub", "TG")))

  dir <- tempdir()
  on.exit(rm_all(ask = FALSE))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  template1 <- 1
  expect_error(save_data(template1, ask = FALSE), "template1 must be a data frame")
  expect_identical(save_data(TG, sub = dir, ask = FALSE), TG)
  expect_identical(load_data("TG", sub = dir), TG)
})
