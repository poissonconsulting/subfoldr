context("md")

test_that("md", {
  main <- file.path(system.file(package = "subfoldr"), "output")
  md_tables(main = main)
})
