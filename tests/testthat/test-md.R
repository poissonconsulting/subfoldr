context("md")

test_that("md", {
  main <- file.path(system.file(package = "subfoldr"), "output")
  files <- md_files(headings = list(character(0), c("2nd" = "Number 2")),
                    drop = list(character(0), character(0), "data2"),
                    main = main, sub = "", nheaders = 2L, header1 = 4L, locale = "en", class = "tables")

  names(files) <- NULL

  expect_identical(files, c("#### First\n##### Number 2", "##### Second", ""))

})
