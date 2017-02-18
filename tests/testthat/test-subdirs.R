context("subdirs")

test_that("subdirs", {
  main <- file.path(system.file(package = "subfoldr"), "output")

  expect_identical(subdirs_tables(main = main, sub = ""), c("first"))
  expect_identical(subdirs_tables(main = main, sub = "first"), c("2nd", "second"))
  expect_identical(subdirs_tables(main = main, sub = "first/2nd"), c("third"))
  expect_identical(subdirs_tables(main = main, sub = "first/2nd/third"), character(0))
  expect_identical(subdirs_tables(main = main, sub = "missing"), character(0))
})
