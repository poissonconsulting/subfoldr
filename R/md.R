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

  TRUE
}

md_files <- function(headings, drop, main, sub, nheaders, header1, locale, class) {

  check_md_args(headings = headings, drop = drop, main = main,
                sub = sub, nheaders = nheaders, header1 = header1, locale = locale, class = class)

  dir <- file.path(main, class, sub)

  files <- list_files(dir)

  if (!length(files)) return(NULL)

  nsub <- max(nsubs(files))

  if (length(drop) > nsub) drop <- drop[1:nsub]
  if (length(headings) > nsub) headings <- headings[1:nsub]
  nheaders %<>% min(nsub)

  subs <- subs_matrix(files)

  drop %<>% drop_rows(subs, .)

  files <- files[!drop]
  subs <- subs[, !drop, drop = FALSE]

  if (!length(files)) return(NULL)

  order <- order_headings(subs, headings, locale)

  subs <- subs[,order,drop = FALSE]
  files <- files[order]

  subs %<>% rename_headings(headings)

  subs %<>% set_headers(nheaders, header1)

  files <- names(files)
  names(files) <- subs
  files
}

md_transfers <- function(headings, drop, main, sub, report, locale, class) {
  dir <- file.path(main, class, sub)

  files <- list_files(dir)

  if (!length(files)) return(character(0))

  nsub <- max(nsubs(files))

  if (length(drop) > nsub) drop <- drop[1:nsub]
  if (length(headings) > nsub) headings <- headings[1:nsub]

  subs <- subs_matrix(files)

  drop %<>% drop_rows(subs, .)

  files <- files[!drop]
  subs <- subs[, !drop, drop = FALSE]

  if (!length(files)) return(character(0))

  order <- order_headings(subs, headings, locale)

  subs <- subs[,order,drop = FALSE]
  files <- files[order]

  subs %<>% rename_headings(headings)

  subs %<>% plyr::alply(2, str_c, collapse = "/") %>% unlist() %>% str_replace("/$", "")

  subs %<>% vapply(capitalize_first_letter_words, "")
  subs %<>% str_c(report, class, ., sep = "/")  %>% str_replace("/$", "")

  subs %<>% str_c(class_ext(class), sep = ".")

  if (sub != "") subs %<>% file.path(sub, .)

  files <- names(files)

  files %<>% str_replace("(_)([^/]+[.]RDS$)", "\\2")
  files %<>% str_replace("RDS$", class_ext(class))

  names(files) <- subs
  files
}

#' Markdown Plots
#'
#' Returns a string of plots in markdown format ready for inclusion in a report.
#'
#' @inheritParams md_tables
#' @export
md_plots <- function(headings = list(character(0)), drop = list(character(0)),
                     main = get_main(), sub = "", report = get_report(),
                     nheaders = 0L, header1 = 3L,
                     locale = "en",
                     ask = getOption("subfoldr.ask", TRUE)) {

  if (!is.null(report) && (!is.character(report) || !length(report) == 1))
    error("report must be NULL or a string")

  check_flag(ask)

  files <- md_files(headings = headings, drop = drop, main = main,
                    sub = sub, nheaders = nheaders,
                    header1 = header1,
                    locale = locale, class = "plots")

  transfers <- md_transfers(headings = headings, drop = drop, main = main,
                            sub = sub, report = report, locale = locale, class = "plots")

  if (!is.null(report)) {
    if (!is.character(report) || !length(report) == 1)
      error("report must be NULL or a string")

    if (!ask || yesno("Copy plots to directory ", report, "?")) {
      transfer_files(transfers)
      csvs <- str_replace(transfers, "[.]png$", ".csv")
      names(csvs) <- str_replace(names(transfers), "[.]png$", ".csv")
      transfer_files(csvs)
    } else  names(transfers) <- transfers
  } else names(transfers) <- transfers

  txt <- NULL
  plotnum <- 0

  for (i in seq_along(files)) {

    plotnum <- plotnum + 1

    file <- files[i]

    info <- readRDS(file)

    caption <- info$caption
    caption %<>% add_full_stop()
    caption %<>% str_c("Figure ", plotnum, ". ", .)

    file %<>% str_replace("(_)([^/]+)(.RDS)", "\\2.rds")

    txt %<>% c(names(files)[i])

    txt %<>% c("\n<figure>") %>%
      c(str_c("<img alt = \"", names(transfers)[i], "\" src = \"", names(transfers)[i],
              "\" title = \"", names(transfers)[i], "\" width = \"", round(info$width / 6 * 100), "%\">")) %>%
      c(str_c("<figcaption>", caption, "</figcaption>")) %>%
      c("</figure>")
  }
  txt %<>% str_c(collapse = "\n")
  txt
}

#' Markdown Tables
#'
#' Returns a string of tables in markdown format ready for inclusion in a report.
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
#' @param report A string indicating the report folder to copy the csv files.
#' @param nheaders An count of the number of headings to assign headers to.
#' @param header1 A count of the heading level for the first header.
#' @param locale A string of the locale.
#' @param ask A string indicating whether to ask before creating a sub directory.
#' @return A string of the report templates in markdown format ready for inclusion in a report.
#' @export
md_tables <- function(headings = list(character(0)), drop = list(character(0)),
                      main = get_main(), sub = "", report = get_report(),
                      nheaders = 0L, header1 = 3L,
                      locale = "en",
                      ask = getOption("subfoldr.ask", TRUE)) {

  if (!is.null(report) && (!is.character(report) || !length(report) == 1))
    error("report must be NULL or a string")

  check_flag(ask)

  files <- md_files(headings = headings, drop = drop, main = main,
                    sub = sub, nheaders = nheaders,
                    header1 = header1,
                    locale = locale, class = "tables")

  transfers <- md_transfers(headings = headings, drop = drop, main = main,
                            sub = sub, report = report, locale = locale, class = "tables")

  if (!is.null(report)) {
    if (!is.character(report) || !length(report) == 1)
      error("report must be NULL or a string")

    if (!ask || yesno("Copy tables to directory ", report, "?")) {
      transfer_files(transfers)
    }
  }

  txt <- NULL
  tabnum <- 0

  for (i in seq_along(files)) {

    tabnum <- tabnum + 1

    file <- files[i]

    caption <- readRDS(file)$caption
    caption %<>% add_full_stop()
    caption %<>% str_c("Table ", tabnum, ". ", .)

    file %<>% str_replace("(_)([^/]+)(.RDS)", "\\2.rds")
    table <- readRDS(file = file)

    txt %<>% c(names(files)[i]) %>% c("")

    txt %<>% c(caption) %>% c("")
    txt %<>% c(knitr::kable(table, format = "markdown", row.names = FALSE))
    txt %<>% c("")
  }
  txt %<>% str_c(collapse = "\n")
  txt
}

#' Markdown Table
#'
#' Returns a string of table in markdown format ready for inclusion in a report.
#'
#' @inheritParams md_tables
#' @param x A string of the table name
#' @param caption A string of the caption.
#' @return A string of the report table in markdown format ready for inclusion in a report.
#' @export
md_table <- function(x, caption = NULL, main = get_main(), sub = get_sub(), report = get_report(),
                      locale = "en",
                      ask = getOption("subfoldr.ask", TRUE)) {

  if (!is.null(report) && (!is.character(report) || !length(report) == 1))
    error("report must be NULL or a string")

  if (!is.null(caption) && (!is.character(caption) || !length(caption) == 1))
    error("caption must be NULL or a string")

  check_flag(ask)

  files <- md_files(headings = list(character(0)), drop = list(character(0)),
                    main = main, sub = sub, nheaders = 0L,
                    header1 = 3L,
                    locale = locale, class = "tables")

  if (!length(files)) return("")

  bol <- str_detect(files, str_c("/_", x, "[.]RDS$"))

  if (!any(bol)) return("")

  stopifnot(sum(bol) == 1)

  transfers <- md_transfers(headings = list(character(0)), drop = list(character(0)), main = main,
                            sub = sub, report = report, locale = locale, class = "tables")

  files <- files[bol]
  transfers <- transfers[bol]

  if (!is.null(report)) {
    if (!is.character(report) || !length(report) == 1)
      error("report must be NULL or a string")

    if (!ask || yesno("Copy table to directory ", report, "?")) {
      transfer_files(transfers)
    }
  }

  txt <- NULL
  tabnum <- 0

  for (i in seq_along(files)) {

    tabnum <- tabnum + 1

    file <- files[i]

    caption <- if(is.null(caption)) readRDS(file)$caption else caption
    caption %<>% add_full_stop()
    caption %<>% str_c("Table ", tabnum, ". ", .)

    file %<>% str_replace("(_)([^/]+)(.RDS)", "\\2.rds")
    table <- readRDS(file = file)

    txt %<>% c(names(files)[i]) %>% c("")

    txt %<>% c(caption) %>% c("")
    txt %<>% c(knitr::kable(table, format = "markdown", row.names = FALSE))
    txt %<>% c("")
  }
  txt %<>% str_c(collapse = "\n")
  txt
}

#' Markdown Templates
#'
#' Returns a string of templates in markdown format ready for inclusion in a report.
#'
#' @inheritParams md_tables
#' @return A string of the report templates in markdown format ready for inclusion in a report.
#' @export
md_templates <- function(headings = list(character(0)), drop = list(character(0)),
                         main = get_main(), sub = "", report = get_report(),
                         nheaders = 0L, header1 = 3L,
                         locale = "en",
                         ask = getOption("subfoldr.ask", TRUE)) {

  if (!is.null(report) && (!is.character(report) || !length(report) == 1))
    error("report must be NULL or a string")

  check_flag(ask)

  files <- md_files(headings = headings, drop = drop, main = main,
                    sub = sub, nheaders = nheaders,
                    header1 = header1,
                    locale = locale, class = "templates")

  transfers <- md_transfers(headings = headings, drop = drop, main = main,
                            sub = sub, report = report, locale = locale, class = "templates")

  if (!is.null(report)) {
    if (!is.character(report) || !length(report) == 1)
      error("report must be NULL or a string")

    if (!ask || yesno("Copy templates to directory ", report, "?")) {
      transfer_files(transfers)
    }
  }

  txt <- NULL
  tempnum <- 0

  for (i in seq_along(files)) {

    tempnum <- tempnum + 1

    file <- files[i]

    caption <- readRDS(file)$caption
    caption %<>% add_full_stop()
    caption %<>% str_c("Template ", tempnum, ". ", .)

    file %<>% str_replace("(_)([^/]+)(.RDS)", "\\2.rds")
    template <- readRDS(file = file)

    txt %<>% c(names(files)[i])

    txt %<>% c("```")
    txt %<>% c(".")
    txt %<>% c(template)
    txt %<>% c("..")
    txt %<>% c("```")
    txt %<>% c(caption)
    txt %<>% c("")
  }
  txt %<>% str_c(collapse = "\n")
  txt
}

#' Markdown Plot
#'
#' Returns a string of plot in markdown format ready for inclusion in a report.
#'
#' @inheritParams md_tables
#' @param x A string of the plot name
#' @param caption A string of the caption.
#' @return A string of the plot in markdown format ready for inclusion in a report.
#' @export
md_plot <- function(x, caption = NULL, main = get_main(), sub = get_sub(), report = get_report(),
                     locale = "en",
                     ask = getOption("subfoldr.ask", TRUE)) {

  if (!is.null(report) && (!is.character(report) || !length(report) == 1))
    error("report must be NULL or a string")

  if (!is.null(caption) && (!is.character(caption) || !length(caption) == 1))
    error("caption must be NULL or a string")

  check_flag(ask)

  files <- md_files(headings = list(character(0)), drop = list(character(0)),
                    main = main, sub = sub, nheaders = 0L,
                    header1 = 3L,
                    locale = locale, class = "plots")

  if (!length(files)) return("")

  bol <- str_detect(files, str_c("/_", x, "[.]RDS$"))

  if (!any(bol)) return("")

  stopifnot(sum(bol) == 1)

  transfers <- md_transfers(headings = list(character(0)), drop = list(character(0)), main = main,
                            sub = sub, report = report, locale = locale, class = "plots")

  files <- files[bol]
  transfers <- transfers[bol]

  if (!is.null(report)) {
    if (!is.character(report) || !length(report) == 1)
      error("report must be NULL or a string")

    if (!ask || yesno("Copy plot to directory ", report, "?")) {
      transfer_files(transfers)
    }
  }

  txt <- NULL
  plotnum <- 0

  for (i in seq_along(files)) {

    plotnum <- plotnum + 1

    file <- files[i]

    info <- readRDS(file)

    caption <- if (is.null(caption)) info$caption else caption
    caption %<>% add_full_stop()
    caption %<>% str_c("Figure ", plotnum, ". ", .)

    file %<>% str_replace("(_)([^/]+)(.RDS)", "\\2.rds")

    txt %<>% c(names(files)[i])

    txt %<>% c("\n<figure>") %>%
      c(str_c("<img alt = \"", names(transfers)[i], "\" src = \"", names(transfers)[i],
              "\" title = \"", names(transfers)[i], "\" width = \"", round(info$width / 6 * 100), "%\">")) %>%
      c(str_c("<figcaption>", caption, "</figcaption>")) %>%
      c("</figure>")
  }
  txt %<>% str_c(collapse = "\n")
  txt
}
