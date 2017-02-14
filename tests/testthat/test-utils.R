context("utils")

test_that("add full stop", {
  expect_identical(add_full_stop("x"), "x.")
  expect_identical(add_full_stop("x."), "x.")
})

test_that("file_path", {
  reset_all()

  expect_identical(file_path(), character(0))
  expect_identical(file_path("x", "z"), "x/z")
  expect_identical(file_path("x", "", "z"), "x/z")
  expect_identical(file_path("x/", "z"), "x/z")
  expect_identical(file_path("x/", "", "z"), "x/z")
})

test_that("sub", {
  expect_identical(get_sub(), "")
  expect_identical(set_sub("x", "z"), "x/z")
  expect_identical(get_sub(), "x/z")
  expect_identical(reset_sub(), "")
  expect_identical(get_sub(), "")
})

test_that("main", {
  expect_identical(get_main(), "output")
  expect_identical(set_main("x", "z"), "x/z")
  expect_identical(get_main(), "x/z")
  expect_identical(reset_main(), "output")
  expect_identical(get_main(), "output")

  reset_all()
})

test_that("all", {
  expect_true(reset_all())
})

test_that("sub_names", {
  expect_identical(sub_names("1")[[1]], "1")
  expect_identical(sub_names("")[[1]], "")
  expect_identical(sub_names("1/3")[[1]], c("1", "3"))
})

test_that("nsubs", {
  expect_identical(nsubs("1"), 1L)
  expect_identical(nsubs("1/3"), 2L)
  expect_identical(nsubs(c("1/3", ".")), c(2L, 1L))
})

test_that("list_files", {
  files <- list_files(file.path(system.file(package = "subfoldr"), "output", "tables"), TRUE)
  names(files) <- NULL # names depend on where run
  expect_identical(files,
                   c("first/2nd/third/TG","first/second/data2",
                     "first/second/mtcars2", "first/second/mtcars3"))
})

test_that("subs_matrix", {
  files <- list_files(file.path(system.file(package = "subfoldr"), "output", "tables"), TRUE)

  expect_identical(subs_matrix(files[1]), matrix(c("first", "2nd", "third", "TG"), ncol = 1))
  expect_identical(subs_matrix(files), matrix(c("first", "2nd", "third", "TG", "first", "second", "data2", "", "first", "second", "mtcars2", "", "first", "second", "mtcars3", ""), ncol = 4))
})

test_that("drop_rows", {
  subs_matrix <- matrix(as.character(1:4), ncol = 2)
  expect_identical(drop_rows(subs_matrix, drop = list(character(0))), c(FALSE, FALSE))
  expect_identical(drop_rows(subs_matrix, drop = list("oeu", "11")), c(FALSE, FALSE))
  expect_error(drop_rows(subs_matrix, drop = list("oeu", "11", "eee")))
  expect_identical(drop_rows(subs_matrix, drop = list("1")), c(TRUE, FALSE))
  expect_identical(drop_rows(subs_matrix, drop = list("2", "1")), c(FALSE, FALSE))
  expect_identical(drop_rows(subs_matrix, drop = list("1", "4")), c(TRUE, TRUE))
})

test_that("rename_heading", {
  expect_identical(rename_heading(1:2, c("1" = "x", "3" = "zz")), c("x", "2"))
})

test_that("rename_headings", {
  subs_matrix <- matrix(as.character(1:4), ncol = 2)
  expect_identical(rename_headings(subs_matrix, headings = list(character(0))), subs_matrix)
  expect_identical(rename_headings(subs_matrix, headings = list(c("1" = "x"))), matrix(as.character(c("x", 2:4)), ncol = 2))
  expect_identical(rename_headings(subs_matrix, headings = list(c("1" = "x", "4" = "zz"))), matrix(as.character(c("x", 2:4)), ncol = 2))
  expect_identical(rename_headings(subs_matrix, headings = list(c("1" = "x"), c("4" = "zz"))), matrix(as.character(c("x", 2:3, "zz")), ncol = 2))
})

test_that("set_headers", {
  subs_matrix <- matrix(as.character(1:4), ncol = 2)

})

test_that("order_heading", {
  expect_identical(order_heading(c("1", "2"), character(0), locale = "en"), c("000001", "000002"))
  expect_identical(order_heading(c("2", "1"), character(0), locale = "en"), c("000002", "000001"))
  expect_identical(order_heading(c("2", "2", "1", "1"), character(0), locale = "en"), c("000002", "000002", "000001", "000001"))
  expect_identical(order_heading(c("1", "2", "this"), c("that" = "Blah", "this" = "This Title"), locale = "en"), c("000002", "000003", "000001"))
})

test_that("order_headings", {
  subs_matrix <- matrix(as.character(1:4), ncol = 2)
  expect_identical(order_headings(subs_matrix, list(character(0)), locale = "en"), c(1L, 2L))
  expect_identical(order_headings(subs_matrix, list(c("5" = "not", "3" = "this")), locale = "en"), c(2L, 1L))
})

