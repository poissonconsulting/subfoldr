context("data")

test_that("data", {
  rm(list = ls())
  expect_identical(ls(), character(0))

  main <- file.path(system.file(package = "subfoldr"), "output")
  sub <- file.path("first", "2nd", "third")

  load_templates(main = main, sub = sub)
  expect_identical(ls(), c("main", "sub", "template_2"))

  template1 <- 1
  expect_error(save_template(template1, ask = FALSE), "template1 must be class character")
  expect_error(load_template("template1"))
})
