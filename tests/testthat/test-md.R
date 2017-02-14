context("md")

test_that("md_files works", {
  main <- file.path(system.file(package = "subfoldr"), "output")

  files <- md_files(headings = list(character(0)),
                    drop = list(character(0), character(0), "data2"),
                    main = main, sub = "", nheaders = 1L, header1 = 3L, locale = "en", class = "tables")

  files <- names(files)

  expect_identical(files, c("### First", "", ""))

  files <- md_files(headings = list(character(0)),
                    drop = list(character(0), character(0), "data2"),
                    main = main, sub = "", nheaders = 2L, header1 = 4L, locale = "en", class = "tables")

  files <- names(files)

  expect_identical(files, c("#### First\n##### 2Nd", "##### Second", ""))

  files <- md_files(headings = list(character(0), c("second" = "Word 2", "2nd" = "Letter 2")),
                    drop = list(character(0), character(0), "data2"),
                    main = main, sub = "", nheaders = 2L, header1 = 4L, locale = "en", class = "tables")

  files <- names(files)

  expect_identical(files, c("#### First\n##### Word 2", "", "##### Letter 2"))
})

test_that("md_tables works", {
  main <- file.path(system.file(package = "subfoldr"), "output")

  md_tables <- md_tables(headings = list(character(0), c("second" = "Word 2", "2nd" = "Letter 2")),
                    drop = list(character(0), character(0), "data2"),
                    main = main, report = NULL, locale = "en")

  expect_identical(datacheckr::check_string(md_tables), md_tables)
})

test_that("md_templates works", {
  main <- file.path(system.file(package = "subfoldr"), "output")

    md_templates <- md_templates(headings = list(character(0), c("second" = "Word 2", "2nd" = "Letter 2")),
                         drop = list(character(0), character(0), "data2"),
                         main = main, report = NULL, locale = "en")

  expect_identical(md_templates, "#### First\n```\n.\nmodel{\ndo stuff\n}\n\n..\n```\nTemplate 1. \n")

  md_templates <- md_templates(headings = list(character(0), c("second" = "Word 2", "2nd" = "Letter 2")),
                               drop = list(character(0), character(0), "data2"),
                               nheaders = 2L,
                               main = main, report = NULL, locale = "en")

  expect_identical(md_templates, "#### First\n##### Letter 2\n```\n.\nmodel{\ndo stuff\n}\n\n..\n```\nTemplate 1. \n")
})

test_that("md_plots works", {
  main <- file.path(system.file(package = "subfoldr"), "output")

  md_plots <- md_plots(headings = list(character(0), c("second" = "Word 2", "2nd" = "Letter 2")),
                               drop = list(character(0), character(0), "data2"),
                               main = main, report = NULL, locale = "en")

  expect_match(md_plots, "^#### First\n\n<figure>\n<img alt = \"")
  expect_match(md_plots, "first/2nd/third/cylmpg.png\" width = \"100%\">\n<figcaption>Figure 1. a fine plot.</figcaption>\n</figure>$")
})
