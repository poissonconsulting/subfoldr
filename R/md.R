check_md_args <- function(headings, drop, main, sub, nheaders, header1, locale, class) {
  check_string(main)
  check_string(sub)
  check_count(nheaders)
  check_count(header1)
  check_string(locale)
  check_string(class)

  if (header1 < 1) error("header1 cannot be less than 1")
  stopifnot(class %in% c("templates", "tables", "objects", "plots"))

  if (!is.list(drop)) error("drop must be a list")
  if (!all(vapply(drop, is.character, TRUE)))
    error("drop must be a list of character vectors")

  if (!is.list(headings)) error("headings must be a list")
  if (!all(vapply(headings, is.character, TRUE)))
    error("headings must be a list of character vectors")
  if (!all(vapply(headings, function(x) !length(x) || !is.null(names), TRUE)))
    error("headings must be a list of named character vectors")

  dir <- file.path(main, class, sub)

  files <- list_files(dir, report = TRUE)

  if (!length(files)) return(TRUE)

  nsub <- max(nsubs(files))

  if (nsub < length(drop)) error("there are more vectors in headings than subfolders")
  if (nsub < length(headings)) error("there are more vectors in headings than subfolders")
  if (nsub < nheaders) error("there are more headers than subfolders")

  TRUE
}

md_files <- function(headings, drop, main, sub, nheaders, header1, locale, class) {

  check_md_args(headings = headings, drop = drop, main = main,
                sub = sub, nheaders = nheaders, header1 = header1, locale = locale, class = class)

  dir <- file.path(main, class, sub)

  files <- list_files(dir, report = TRUE)

  if (!length(files)) return(NULL)

  subs <- subs_matrix(files)

  drop %<>% drop_rows(subs, .)

  files <- files[!drop]
  subs <- subs[, !drop, drop = FALSE]

  if (!length(files)) return(NULL)

  order <- order_headings(subs, headings, locale)
  subs <- subs[,order,drop = FALSE]
  files <- files[order]

  subs %<>% rename_headings(headings)
  subs %<>% set_headers(nheaders, header1, locale)

  files <- names(files)
  names(files) <- subs
  files
}

#' Markdown Templates
#'
#' Returns a string of templates in markdown format ready for inclusion in a report.
#'
#' The names in the character vectors in headings indicate the new headings for each subfolder.
#' By default missing subfolders receive their current name with the first letter of each word capitalized.
#' Subfolders with the name "" do not receive a heading.
#' The order of the subfolders indicates the order in which they should appear.
#' By default missing subfolders appear in alphabetical order.
#' The first named character vector is applied to the highest level of subfolders starting at sub and so on.
#' The number of character vectors indicates the number of levels that should receive headings.
#' By default the highest level of subfolders are considered to be third order headings.
#'
#' The elements in the character vectors in drop indicate the subfolders to exclude from the report.
#' Again the number of the character vector indicates the level to which it applies.
#'
#' @param headings A list of named character vectors.
#' @param drop A list of character vectors specify the subfolders to drop.
#' @param main A string of the main subfolder.
#' @param sub A string of the path to the subfolders to save the object (by default = "").
#' @param nheaders An count of the number of headings to assign headers to.
#' @param header1 A count of the heading level for the first header.
#' @param locale A string of the locale.
#' @param ask A string indicating whether to ask before creating a sub directory.
#' @return A string of the report templates in markdown format ready for inclusion in a report.
#' @export
md_tables <- function(headings = list(character(0)), drop = list(character(0)),
                      main = get_main(), sub = "",
                      nheaders = 1L, header1 = 3L,
                      locale = "en",
                      ask = getOption("subfoldr.ask", TRUE)) {

  files <- md_files(headings = headings, drop = drop, main = main,
           sub = sub, nheaders = nheaders,
           header1 = header1,
           locale = locale, class = "tables")

  txt <- NULL
  tabnum <- 0

  for (i in seq_along(files)) {

    tabnum <- tabnum + 1

    file <- files[i]

    caption <- readRDS(file)$caption
    caption %<>% add_full_stop()
    caption %<>% str_c("Table ", tabnum, ". ", .)

    file %<>% str_replace("([.])(\\w+)(.RDS)", "\\2.rds")
    table <- readRDS(file = file)

    txt %<>% c(names(files)[i])

    txt <- c(txt, knitr::kable(table, format = "markdown", row.names = FALSE, caption = caption))
    txt %<>% c("")
  }
  txt %<>% str_c(collapse = "\n")
  txt
}
