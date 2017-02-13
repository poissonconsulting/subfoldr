md_files <- function(headings, drop, main, sub, report, headers, locale, class) {

  check_md_args(headings = headings, drop = drop, main = main,
                sub = sub, report = report, headers = headers, locale = locale, class = class)

  dir <- file.path(main, class, sub)

  files <- list_files(dir, report = TRUE)

  if (!length(files)) return(NULL)

  subs <- subs_matrix(files)

  drop %<>% drop_rows(subs, .)

  files <- files[!drop]
  subs <- subs[, !drop, drop = FALSE]

  if (!length(files)) return(NULL)

  # order <- order_headings(subs, headings)
  # subs <- subs[order,,drop = FALSE]
  # files <- files[order]

  subs %<>% rename_headings(headings)
  subs %<>% apply(MARGIN = 2, str_to_title, locale = locale)

  subs %<>% set_headers(headers)


  # do heading numbers

  subs %<>% plyr::alply(2, str_c, collapse = "/") %>% unlist()
  subs %<>% str_replace("/*$", "") %>% str_replace("/[^/]+$", "")

  names(subs) <- names(files)
  subs
 # files
 # subs
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
#' @param report A string of the path to the report folder.
#' @param main A string of the main subfolder.
#' @param sub A string of the path to the subfolders to save the object (by default = "").
#' @param headers An integer vector of length 2 specify the header level for the first subfolders and the numer
#' @param locale A string of the locale.
#' @param ask A string indicating whether to ask before creating a sub directory.
#' @return A string of the report templates in markdown format ready for inclusion in a report.
#' @export
md_tables <- function(headings = list(character(0)), drop = list(character(0)),
                      report = get_report(), main = get_main(), sub = "",
                      headers = c(3L,3L),
                      locale = "en",
                      ask = getOption("subfoldr.ask", TRUE)) {

  md_files(headings = headings, drop = drop, main = main,
           sub = sub, report = report, headers = headers,
           locale = locale, class = "tables")

  #
  # to <- str_replace(files, str_c("(", dir, "/)(.*)"), "report/tables/\\2")
  #
  # for (i in seq_along(files)) {
  #   if (!dir.exists(dirname(to[i]))) dir.create(dirname(to[i]), recursive = TRUE)
  #   file.copy(from = files[i], to = to[i], overwrite = TRUE)
  # }
  #
  # files %<>% rename_headings(rename)
  #
  # names(files) <- str_replace_all(names(files), "([^/]*/$)", "")
  # names(files) <- replace_slashes(names(files))
  #
  # txt <- ""
  #
  # tabnum <- 0
  #
  # previous_heading <- NULL
  #
  # for (i in seq_along(files)) {
  #
  #   file <- str_replace(files[i], "[.]csv$", ".RDS")
  #   file %<>% str_replace("(.*/)(\\w+[.]RDS$)", "\\1.\\2")
  #
  #   heading <- names(files[i])
  #
  #   table <- readRDS(file = file)
  #
  #   tabnum <- tabnum + 1
  #
  #   if ((is.null(previous_heading) || !identical(previous_heading, heading)) && !identical(heading, ""))
  #     txt %<>% c(paste("\n####", heading))
  #
  #   previous_heading <- heading
  #
  #   caption <- str_replace(table$caption, "([^.]$)", "\\1.")
  #   caption %<>% str_c("Table ", tabnum, ". ", .)
  #
  #   txt %<>% c("", knitr::kable(table$data, row.names = FALSE, output = FALSE, caption = caption), "")
  # }
  # txt %<>% str_c(collapse = "\n")
  # invisible(txt)
}


