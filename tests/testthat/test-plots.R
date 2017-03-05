context("plots")

test_that("plots", {
  rm(list = ls())
  expect_identical(ls(), character(0))

  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("first", "2nd", "third")

  load_plots(main = main, sub = sub)
  expect_identical(ls(), c("cylmpg", "main", "sub"))

  expect_is(cylmpg, "ggplot")

  plots <- load_plot_recursive("cylmpg", main = main, sub = "first")
  expect_is(plots, "list")
  expect_identical(length(plots), 1L)
  expect_true(ggplot2::is.ggplot(plots[[1]]))

  plots <- load_plot_data_recursive("cylmpg", main = main, sub = "first")
  expect_identical(nrow(plots[[1]]), 32L)
})
