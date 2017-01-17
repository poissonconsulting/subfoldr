#' Save Object
#'
#' @param x The object to save. If missing saves all objects in calling env.
#' @param type A string of the type of x.
#' @param main A string of the main subfolder.
#' @param sub A string of the path to the directory to save the object.
#' @param x_name An optional string of the name to use.
#' @param ask A string indicating whether to ask before creating a sub directory.
#' @return The object x or TRUE or FALSE is x is missing.
#' @export
save_object <- function(x, type = "", main = get_main(), sub = get_sub(),
                        x_name = NULL, ask = getOption("subfoldr.ask", TRUE)) {
  check_string(type)
  check_string(main)
  check_string(sub)
  check_flag(ask)

  if (missing(x)) {
    names <- objects(envir = calling_env())
    if (!length(names)) {
      warning("no objects in calling environment")
      return(invisible(FALSE))
    }
    for (name in names) {
      object <- get(x = name, envir = calling_env())
      save_object(object, type = type, main = main, sub = sub, x_name = name, ask = ask)
    }
    return(invisible(TRUE))
  }

  if (is.null(x_name)) x_name <- deparse(substitute(x))

  check_string(x_name)

  dir <- file_path(main, "objects", type, sub)

  create_dir(dir, ask)

  file <- file_path(dir, x_name) %>% str_c(".rds")

  saveRDS(x, file)

  invisible(x)
}

#' Load Object
#'
#' @inheritParams save_object
#' @export
load_object <- function(x, type = "", main = get_main(), sub = get_sub(), env = calling_env()) {
  load_rds(x, class = "objects", main = main, sub = sub, env = env)
}
