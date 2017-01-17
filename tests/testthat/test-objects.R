context("rds")

test_that("rds", {

  reset_all()

  set_main(tempdir())

  data1 <- 1
  data2 <- data.frame(a = 3:4)

  save_object(data1, ask = FALSE)
  expect_identical(load_object("data1"), data1)

  expect_error(load_object("data2"))
  # expect_true(save_object())
  # rm(data1, data2)
  #
  # expect_identical(load_object("data1"), 1)
  # expect_identical(load_object("data2"), data.frame(a = 3:4))
  #
  # expect_identical(ls(), character(0))
  #
  # load_object()
  # expect_identical(ls(), c("data1", "data2"))
  # expect_identical(data2, data.frame(a = 3:4))
  # expect_identical(load_object("data2"), data2)
})
