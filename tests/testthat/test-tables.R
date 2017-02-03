context("tables")

test_that("tables", {

  reset_all()

  set_main(tempdir())

  data1 <- 1
  data2 <- data.frame(a = 3:4)

  expect_error(save_table(data1, caption = "A table", ask = FALSE), "data1 must be a data.frame")
  expect_error(load_table("data2"))
  expect_identical(save_table(data2, ask = FALSE), data2)
  expect_identical(load_table("data2"), data2)

  rm(list = ls())

  load_table()

  expect_identical(ls(), "data2")
})
