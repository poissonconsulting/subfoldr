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
    flag <- FALSE
    for (x_name in names) {
      x <- get(x = x_name, envir = calling_env())
      if (!is.function(x))
        save_rds(x, "objects", type = type, main = main, sub = sub, x_name = x_name, ask = ask)
      flag <- TRUE
    }
    if (!flag) warning("no 'objects' in calling environment")
    return(invisible(flag))
  }

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)

  save_rds(x, "objects", type = type, main = main, sub = sub, x_name = x_name, ask = ask)
}

#' Load Object
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into if x is missing.
#' @export
load_object <- function(x, type = "", main = get_main(), sub = get_sub(), env = calling_env()) {
  load_rds(x, class = "objects", type = type, main = main, sub = sub, env = env)
}
