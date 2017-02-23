context("tables")

test_that("tables", {
  rm(list = ls())
  expect_identical(ls(), character(0))

  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("first", "second")

  load_table(main = main, sub = sub)
  expect_identical(ls(), c("data2", "main", "mtcars", "mtcars2", "mtcars3", "sub"))

  expect_is(data2, "data.frame")

  data1 <- 1

  expect_error(save_table(data1, caption = "A table", ask = FALSE), "data1 must be a data.frame")
  expect_error(load_table("data3"))

  expect_error(load_tables())
  expect_error(load_tables("data3"))
  mtcars <- load_tables("mtcars", main = main)

  expect_identical(names(mtcars), c("first/2nd/third", "first/second"))
  mtcars %<>% dplyr::bind_rows()
  expect_identical(colnames(mtcars), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "Subfolder1" , "Subfolder2", "Subfolder3"))
  expect_identical(nrow(mtcars), 64L)

  mtcars <- load_tables("mtcars", main = main, sub = "first")
  expect_identical(names(mtcars), c("first/2nd/third", "first/second"))
  mtcars %<>% dplyr::bind_rows()
  expect_identical(colnames(mtcars), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "Subfolder1" , "Subfolder2"))
  expect_identical(nrow(mtcars), 64L)

  mtcars <- load_tables("mtcars", main = main, sub = "first/second")
  expect_identical(names(mtcars), c("first/second"))
  mtcars %<>% dplyr::bind_rows()
  expect_identical(colnames(mtcars), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
  expect_identical(nrow(mtcars), 32L)
})
