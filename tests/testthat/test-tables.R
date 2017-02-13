context("tables")

test_that("tables", {
  rm(list = ls())
  expect_identical(ls(), character(0))

  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("first", "second")

  load_table(main = main, sub = sub)
  expect_identical(ls(), c("data2", "main", "mtcars", "sub"))

  expect_is(data2, "data.frame")

  data1 <- 1

  expect_error(save_table(data1, caption = "A table", ask = FALSE), "data1 must be a data.frame")
  expect_error(load_table("data3"))

  md_tables(main = main)
})
