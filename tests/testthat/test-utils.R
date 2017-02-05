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
  expect_identical(set_sub("x", "z"), "x/z")
  expect_identical(get_sub(), "x/z")
  expect_identical(reset_sub(), "")
  expect_identical(get_sub(), "")

  expect_identical(add_sub("y"), "y")
  expect_identical(add_sub("t"), "y/t")
  expect_identical(add_sub(""), "y/t")
  expect_identical(get_sub(), "y/t")
})

test_that("main", {
  expect_identical(get_main(), "output")
  expect_identical(set_main("x", "z"), "x/z")
  expect_identical(get_main(), "x/z")
  expect_identical(reset_main(), "output")
  expect_identical(get_main(), "output")

  reset_all()
})

test_that("all", {
  expect_true(reset_all())
})

test_that("sub_names", {
  expect_identical(sub_names("1")[[1]], "1")
  expect_identical(sub_names("")[[1]], "")
  expect_identical(sub_names("1/3")[[1]], c("1", "3"))
})

test_that("nsubs", {
  expect_identical(nsubs("1"), 1L)
  expect_identical(nsubs("1/3"), 2L)
  expect_identical(nsubs(c("1/3", ".")), c(2L, 1L))
})
