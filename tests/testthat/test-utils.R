context("utils")

test_that("file_path", {
  reset_all()

  expect_identical(file_path(), character(0))
  expect_identical(file_path("x", "z"), "x/z")
  expect_identical(file_path("x", "", "z"), "x/z")
  expect_identical(file_path("x/", "z"), "x/z")
  expect_identical(file_path("x/", "", "z"), "x/z")
})

test_that("sub", {
  expect_identical(get_sub(), "")
  expect_identical(set_sub("x", "z"), "")
  expect_identical(get_sub(), "x/z")
  expect_identical(reset_sub(), "x/z")
  expect_identical(get_sub(), "")

  expect_identical(add_sub("y"), "")
  expect_identical(add_sub("t"), "y")
  expect_identical(add_sub(""), "y/t")
  expect_identical(get_sub(), "y/t")
})

test_that("main", {
  expect_identical(get_main(), "output")
  expect_identical(set_main("x", "z"), "output")
  expect_identical(get_main(), "x/z")
  expect_identical(reset_main(), "x/z")
  expect_identical(get_main(), "output")

  reset_all()
})

test_that("all", {
  expect_true(reset_all())
})
