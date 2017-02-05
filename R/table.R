#' Save Object as .csv
#'
#' @inheritParams save_object
#' @param caption A string of the figure caption.
#' @param report A flag indicating to include the plot in reports (not yet implemented).
#' @return The object x.
#' @export
save_table <- function(x, x_name = NULL, caption = "", report = !identical(caption, ""),
                       main = get_main(), sub = get_sub(),
                       ask = getOption("subfoldr.ask", TRUE)) {

  check_flag(report)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)

  if (!is.data.frame(x)) error(x_name, " must be a data.frame")

  save_rds(x, "tables", main = main, sub = sub, x_name = x_name, ask = ask)

  obj <- list(data = x, caption = caption, report = report)
  file <- file_path(main, "tables", sub, str_c(".", x_name)) %>% str_c(".RDS")
  saveRDS(obj, file = file)

  file <- file_path(main, "tables", sub, x_name) %>% str_c(".csv")
  readr::write_csv(x, path = file)

  invisible(x)
}

#' Load table
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into if x is missing.
#' @export
load_table <- function(x, main = get_main(), sub = get_sub(), env = calling_env()) {
  load_rds(x, class = "tables", main = main, sub = sub, env = env)
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
#' @param report A flag indicating whether to include templates saved with report = TRUE.
#' @param main A string of the main subfolder.
#' @param sub A string of the path to the subfolders to save the object (by default = "").
#' @param ask A string indicating whether to ask before creating a sub directory.
#' @return A string of the report templates in markdown format ready for inclusion in a report.
#' @export
md_tables <- function(headings = list(character(0)), drop = list(character(0)),
                         report = TRUE, main = get_main(), sub = "",
                         ask = getOption("subfoldr.ask", TRUE)) {

  check_md_args(headings = headings, drop = drop, report = report, main = main,
                sub = sub, class = "tables", ask = ask)

  dir <- file.path(main, "tables", sub)

  files <- list_files(dir, report)

  if (!length(files)) return(NULL)

  files

  # further dropping....
  # for()
  #
  # files <- files[!str_replace(names(files),"(^[^/]+)(.*)", "\\1") %in% drop]

  # if (!length(files)) return(NULL)
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

