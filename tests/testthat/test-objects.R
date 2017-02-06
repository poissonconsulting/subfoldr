context("objects")

test_that("objects", {

  reset_all()

  set_main(tempdir())

  data1 <- 1
  data2 <- data.frame(a = 3:4)

  expect_true(save_object(ask = FALSE))
  expect_error(load_object("data1"))
  expect_identical(load_object("data2"), data2)

  expect_identical(save_object(data1, ask = FALSE), data1)
  expect_identical(load_object("data1"), data1)


  rm(data1, data2)

  expect_identical(load_object("data1"), 1)
  expect_identical(load_object("data2"), data.frame(a = 3:4))
  expect_true(is.data.frame(load_object("data2")))

  expect_identical(ls(), character(0))

  load_object()
  expect_identical(ls(), c("data2"))
  expect_identical(data2, data.frame(a = 3:4))
  expect_identical(load_object("data2"), data2)

  expect_identical(object_subdirs(), character(0))
  save_object(data2, sub = "sub", ask = FALSE)
  expect_identical(object_subdirs(), "sub")
})
