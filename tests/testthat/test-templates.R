context("templates")

test_that("templates", {

  reset_all()

  set_main(tempdir())

  template1 <- 1
  template2 <- "
    model{
      do stuff
    }
  "
  expect_error(save_template(template1, ask = FALSE), "template1 must be of class 'character'")
  expect_error(load_template("template1"))
  expect_identical(save_template(template2, ask = FALSE), template2)
  expect_identical(load_template("template2"), template2)

  rm(list = ls())

  load_template()

  expect_identical(ls(), "template2")
})
