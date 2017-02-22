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

load_rdss <- function(x, main, sub, class, data = TRUE, fun = identity) {
  check_string(x)
  check_string(main)
  check_string(sub)
  check_flag(data)

  dir <- file_path(main, class, sub)
  if (!dir.exists(dir)) error("directory '", dir, "' does not exist")

  pattern <- str_c(x, ".rds$")

  files <- list.files(dir, pattern = pattern, recursive = TRUE)
  if (!length(files)) {
    warning("no files with pattern ", pattern, " found")
    return(invisible(FALSE))
  }
  subs <- subs_matrix(files) %>% t()
  subs %<>% plyr::aaply(1, function(x) {x[max(which(!str_detect(x, "^$")))] <- ""; x},
                        .drop = FALSE)

  files %<>% str_c(dir, "/", .)
  files %<>% lapply(readRDS)
  files %<>% lapply(fun)

  if (data) {
    bool <- vapply(files, is.data.frame, TRUE)

    if (!any(bool)) {
      warning("no suitable .rds objects found")
      return(invisible(FALSE))
    }
    files <- files[bool]
    subs <- subs[bool,,drop = FALSE]
  }


  if (ncol(subs) > 1) {
    subs <- subs[, -ncol(subs), drop = FALSE]
    subs %<>% as.data.frame()
    colnames(subs) <- str_c("Subfolder", 1:ncol(subs))
    subs %<>% plyr::alply(.margins = 1, function(x) x)
    if (data) files %<>% purrr::map2(subs, merge)
    subs %<>% dplyr::bind_rows() %>% as.matrix()
    subs %<>% plyr::alply(.margins = 1, str_c, collapse = "/")
    subs %<>% str_replace_all("//", "/") %>% str_replace("/$", "")
    names(files) <- unlist(subs)
  }
  files
}

#' Load Object
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into if x is missing.
#' @export
load_object <- function(x, main = get_main(), sub = get_sub(), is = is.data.frame, env = calling_env()) {
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

#' Load objects
#'
#' List of all data objects in sub and its subdirectories.
#'
#' @inheritParams save_object
#' @param data A flag indicating whether to only load objects inheriting from data.frame.
#' @export
load_objects <- function(x, main = get_main(), sub = "", data = TRUE) {
  load_rdss(x = x, main = main, sub = sub, class = "objects", data = data)
}

#' Load tables
#'
#' List of all tables in sub and its subdirectories.
#'
#' @inheritParams save_object
#' @export
load_tables <- function(x, main = get_main(), sub = "") {
  load_rdss(x = x, main = main, sub = sub, class = "tables")
}

#' Load plots
#'
#' List of all plot data in sub and its subdirectories.
#'
#' @inheritParams save_object
#' @param data A flag indicating whether to load the plot data.
#' @export
load_plots <- function(x, main = get_main(), sub = "", data = FALSE) {
  fun <- if (data) function(x) x$data else identity
  load_rdss(x = x, main = main, sub = sub, class = "plots", data = data, fun = fun)
}
