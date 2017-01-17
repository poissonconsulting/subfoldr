context("rds")

test_that("rds", {

  x <- data.frame()
  y <- data.frame(a = 1)



  expect_true(setup(tempdir(), ask = FALSE))
})
