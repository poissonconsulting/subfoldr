context("objects")

test_that("objects", {

  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("missing")
  expect_false(object_exists("data2", main = main, sub = sub))
})
