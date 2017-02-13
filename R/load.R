load_rds <- function(x, class, main, sub, is = function(x) {TRUE}, env) {

  if (!missing(x)) {
    check_string(x)
    file <- file_path(main, class, sub, x) %>% str_c(".rds")
    if (!file.exists(file)) error("file '", file, "' does not exist")
    return(readRDS(file))
  }

  files <- list.files(path = file_path(main, class, sub), pattern = "[.]rds$")

  flag <- FALSE
  for (file in files) {
    x <- basename(file) %>% str_replace("[.]rds$", "")
    object <- readRDS(file_path(main, class, sub, basename(file)))
    if (is(object)) {
      assign(x, object, envir = env)
      flag <- TRUE
    }
  }
  if (!flag)
    warning("no suitable .rds objects found")
  invisible(flag)
}

#' Load Object
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into if x is missing.
#' @export
load_object <- function(x, main = get_main(), sub = get_sub(),
                        is = is.data.frame, env = calling_env()) {
  load_rds(x, class = "objects", main = main, sub = sub, is = is, env = env)
}

#' Load table
#'
#' @inheritParams save_object
#' @param data A flag indicating whether to load the plot data.
#' @param env The environment to load the objects into if data = TRUE and  x is missing.
#' @export
load_plot <- function(x, main = get_main(), sub = get_sub(), data = FALSE, env = calling_env()) {
  check_flag(data)

  if (!data) return(load_rds(x, class = "plots", main = main, sub = sub, env = env))

  check_string(x)

  plot <- load_rds(x, class = "plots", main = main, sub = sub, env = env)
  plot$data
}

#' Load table
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into if x is missing.
#' @export
load_table <- function(x, main = get_main(), sub = get_sub(), env = calling_env()) {
  load_rds(x, class = "tables", main = main, sub = sub, env = env)
}

#' Load template
#'
#' @inheritParams save_object
#' @param env The environment to load the templates into if x is missing.
#' @export
load_template <- function(x, main = get_main(), sub = get_sub(), env = calling_env()) {
  load_rds(x, class = "templates", main = main, sub = sub, env = env)
}
