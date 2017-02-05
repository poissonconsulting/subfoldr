#' Save string as .txt
#'
#' @inheritParams save_object
#' @param caption A string of the template caption.
#' @param report A flag indicating to include the plot in reports (not yet implemented).
#' @return The object x.
#' @export
save_template <- function(x, x_name = NULL, caption = "", report = !identical(caption, ""),
                          main = get_main(), sub = get_sub(),
                          ask = getOption("subfoldr.ask", TRUE)) {

  check_flag(report)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x, x_name = x_name)
  check_string(x_name)

  save_rds(x, "templates", main = main, sub = sub, x_name = x_name, ask = ask)

  obj <- list(template = x, caption = caption, report = report)
  file <- file_path(main, "templates", sub, str_c(".", x_name)) %>% str_c(".RDS")
  saveRDS(obj, file = file)

  file <- file_path(main, "templates", sub, x_name) %>% str_c(".txt")
  cat(x, file = file)

  invisible(x)
}

#' Load template
#'
#' @inheritParams save_object
#' @param env The environment to load the templates into if x is missing.
#' @export
load_template <- function(x, main = get_main(), sub = get_sub(), env = calling_env()) {
  load_rds(x, class = "templates", main = main, sub = sub, env = env)
}

#' List Template Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
template_subdirs <- function(main = get_main(), sub = get_sub()) {
  subdirs("templates", main = main, sub = sub)
}
