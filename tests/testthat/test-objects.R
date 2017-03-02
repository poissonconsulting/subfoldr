context("objects")

test_that("objects", {

  expect_identical(ls(), character(0))

  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("first", "2nd", "third")

  load_object(main = main, sub = sub)
  expect_identical(ls(), c("data2", "main", "mtcars", "sub"))

  rm(list = ls())
  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("also1")

  load_object(main = main, sub = sub)
  expect_identical(ls(), c("data2", "main", "mtcars", "sub"))
  load_object(main = main, sub = sub, is = function(x) TRUE)
  expect_identical(ls(), c("data2", "main", "main2", "mtcars", "sub", "sub2", "x"))

  expect_true(object_exists("data2", main = main, sub = sub))

  sub <- file.path("missing")
  expect_warning(load_object(main = main, sub = sub))
  expect_false(object_exists("data2", main = main, sub = sub))
})
