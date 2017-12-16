context("plots")

test_that("plots", {
  rm(list = ls())
  expect_identical(ls(), character(0))

  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("first", "2nd", "third")

  load_plots(main = main, sub = sub)
  expect_identical(ls(), c("cyl_mpg", "main", "sub"))

  expect_is(cyl_mpg, "ggplot")

  plots <- load_plot_recursive("cyl_mpg", main = main, sub = "first")
  expect_is(plots, "list")
  expect_identical(length(plots), 1L)
  expect_true(ggplot2::is.ggplot(plots[[1]]))

  plots <- load_plot_data_recursive("cyl_mpg", main = main, sub = "first")
  expect_identical(nrow(plots[[1]]), 32L)


  dir <- tempdir()
  on.exit(rm_all(ask = FALSE))
  ggplot2::qplot(data = mtcars, mpg, binwidth = 1)
  save_plot("mtcars", sub = dir, ask = FALSE)
  expect_identical(load_plot_data("mtcars", sub = dir), mtcars)
})
