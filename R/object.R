#' Save Object as .rds
#'
#' @param x The object to save. If missing saves all objects in global env.
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
    names <- objects(envir = .GlobalEnv)
    if (!length(names)) {
      warning("no objects in global environment")
      return(invisible(FALSE))
    }
    for (name in names) {
      object <- get(x = name, envir = .GlobalEnv)
      save_object(object, type = type, main = main, sub = sub, x_name = name, ask = ask)
    }
    return(invisible(TRUE))
  }

  if (is.null(x_name)) x_name <- deparse(substitute(x))

  check_string(x_name)

  dir <- file_path(main, "object", type, sub)

  if (!dir.exists(dir)) {
    if (ask && !yesno("Create directory '", dir, "'?"))
      return(invisible(x))
    dir.create(dir, recursive = TRUE)
  }

  file <- file_path(dir, x_name) %>% str_c(".rds")

  saveRDS(x, file)

  invisible(x)
}

#' Load Object
#'
#' @inheritParams save_object
#' @export
load_object <- function(x_name = NULL, type = "", main = get_main(), sub = get_sub()) {

  if (!is.null(x_name)) {
    file <- file_path(main, "object", type, sub, x_name) %>% str_c(".rds")
    if (!file.exists(file)) error("file '", file, "' does not exist")
    return(readRDS(file))
  }

  files <- list.files(path = file_path(main, "object", type, sub), pattern = "[.]rds$")
  if (!length(files)) {
    warning("no .rds objects found")
    return(invisible(FALSE))
  }

  for (file in files) {
    x_name <- basename(file) %>% str_replace("[.]rds$", "")
    assign(x_name, load_object(x_name, type = type, main = main, sub = sub), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
