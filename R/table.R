#' Save Object as .csv
#'
#' @inheritParams save_object
#' @return The object x or TRUE or FALSE is x is missing.
#' @export
save_table <- function(x, type = "", main = get_main(), sub = get_sub(),
                       x_name = NULL, ask = getOption("subfoldr.ask", TRUE)) {

  check_string(type)
  check_string(main)
  check_string(sub)
  check_flag(ask)

  if (missing(x)) {
    names <- objects(envir = calling_env())
    flag <- FALSE
    for (x_name in names) {
      x <- get(x = x_name, envir = calling_env())
      if (is.data.frame(x))
        save_table(x, type = type, main = main, sub = sub, x_name = x_name, ask = ask)
        flag <- TRUE
    }
    if (!flag) warning("no data.frames in calling environment")
    return(invisible(flag))
  }

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)

  if (!is.data.frame(x)) error(x_name, " must be a data.frame")

  save_rds(x, "tables", type = type, main = main, sub = sub, x_name = x_name, ask = ask)

  file <- file_path(main, "tables", type, sub, x_name) %>% str_c(".csv")

  readr::write_csv(x, file)

  invisible(x)
}

#' Load table
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into if x is missing.
#' @export
load_table <- function(x, type = "", main = get_main(), sub = get_sub(), env = calling_env()) {
  load_rds(x, class = "tables", type = type, main = main, sub = sub, env = env)
}
