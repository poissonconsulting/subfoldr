#' Save Object as .csv
#'
#' @inheritParams save_object
#' @param caption A string of the figure caption.
#' @return The object x.
#' @export
save_table <- function(x, main = get_main(), sub = get_sub(),
                       x_name = NULL, caption = "",
                       ask = getOption("subfoldr.ask", TRUE)) {

  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)

  if (!is.data.frame(x)) error(x_name, " must be a data.frame")

  save_rds(x, "tables", main = main, sub = sub, x_name = x_name, ask = ask)

  obj <- list(data = x, caption = caption)
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
