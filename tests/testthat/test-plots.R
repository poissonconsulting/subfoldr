context("plots")

test_that("plots", {
  rm(list = ls())
  expect_identical(ls(), character(0))

  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("first", "2nd", "third")

  load_plot(main = main, sub = sub)
  expect_identical(ls(), c("cylmpg", "main", "sub"))

  expect_is(cylmpg, "ggplot")
})
