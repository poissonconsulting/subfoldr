load_rds <- function(x, class, sub, main) {
  check_string(x)
  file <- file_path(main, class, sub, x) %>% str_c(".rds")
  if (!file.exists(file)) error("file '", file, "' does not exist")
  readRDS(file)
}

#' Load Data
#'
#' @inheritParams save_object
#' @export
load_data <- function(x, sub = get_sub(), main = get_main()) {
  load_rds(x, class = "data", sub = sub, main = main)
}

#' Load Object
#'
#' @inheritParams save_object
#' @export
load_object <- function(x, sub = get_sub(), main = get_main()) {
  load_rds(x, class = "objects", sub = sub, main = main)
}

#' Load table
#'
#' @inheritParams save_object
#' @export
load_plot <- function(x, sub = get_sub(), main = get_main()) {
  load_rds(x, class = "plots", sub = sub, main = main)
}

#' Load table
#'
#' @inheritParams save_object
#' @export
load_plot_data <- function(x, sub = get_sub(), main = get_main()) {
  plot <- load_plot(x = x, sub = sub, main = main)
  plot$data
}

#' Load table
#'
#' @inheritParams save_object
#' @export
load_table <- function(x, sub = get_sub(), main = get_main()) {
  load_rds(x, class = "tables", sub = sub, main = main)
}

#' Load template
#'
#' @inheritParams save_object
#' @export
load_template <- function(x, sub = get_sub(), main = get_main()) {
  load_rds(x, class = "templates", sub = sub, main = main)
}

load_rdss <- function(class, sub, main, env, fun = identity) {
  files <- list.files(path = file_path(main, class, sub), pattern = "[.]rds$")

  if (!length(file)) {
    warning("no suitable objects found")
    return(invisible(FALSE))
  }
  for (file in files) {
    x <- basename(file) %>% str_replace("[.]rds$", "")
    object <- readRDS(file_path(main, class, sub, basename(file)))
    object %<>% fun()
    assign(x, object, envir = env)
  }
  invisible(TRUE)
}

#' Load Data
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into
#' @export
load_datas <- function(sub = get_sub(), main = get_main(), env = calling_env()) {
  load_rdss(class = "data", sub = sub, main = main, env = env)
}

#' Load Objects
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into
#' @export
load_objects <- function(sub = get_sub(), main = get_main(), env = calling_env()) {
  load_rdss(class = "objects", sub = sub, main = main, env = env)
}

#' Load Data
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into
#' @export
load_plots <- function(sub = get_sub(), main = get_main(), env = calling_env()) {
  load_rdss(class = "plots", sub = sub, main = main, env = env)
}

#' Load Plot Data
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into
#' @export
load_plot_datas <- function(sub = get_sub(), main = get_main(), env = calling_env()) {
  load_rdss(class = "plots", sub = sub, main = main, env = env, fun = function(x) x$data)
}

#' Load Data
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into
#' @export
load_tables <- function(sub = get_sub(), main = get_main(), env = calling_env()) {
  load_rdss(class = "tables", sub = sub, main = main, env = env)
}

#' Load Templates
#'
#' @inheritParams save_object
#' @param env The environment to load the objects into
#' @export
load_templates <- function(sub = get_sub(), main = get_main(), env = calling_env()) {
  load_rdss(class = "templates", sub = sub, main = main, env = env)
}

load_rds_recursive <- function(x, class, sub, main, top, fun = identity,
                               subfolder_names = FALSE) {
  check_string(x)
  check_string(main)
  check_string(sub)
  check_flag(top)
  check_flag(subfolder_names)

  dir <- file_path(main, class, sub)
  if (!dir.exists(dir)) error("directory '", dir, "' does not exist")

  pattern <- str_c(x, ".rds$")

  files <- list.files(dir, pattern = pattern, recursive = TRUE)
  if(!top) files <- files[grepl("/", files)]

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

  if (ncol(subs) > 1) {
    subs <- subs[, -ncol(subs), drop = FALSE]
    subs %<>% as.data.frame()
    colnames(subs) <- str_c("Subfolder", 1:ncol(subs))
    subs %<>% plyr::alply(.margins = 1, function(x) x)
    if (subfolder_names) files %<>% purrr::map2(subs, merge, by = NULL)
    subs %<>% dplyr::bind_rows() %>% as.matrix()
    subs %<>% plyr::alply(.margins = 1, str_c, collapse = "/")
    subs %<>% str_replace_all("//", "/") %>% str_replace("/$", "")
    names(files) <- unlist(subs)
    if (sub != "") names(files) %<>% file_path(sub, .)
  } else if (sub != "")
    names(files) <- sub
  files
}

#' Load Data Recursive
#'
#' @inheritParams save_object
#' @param top A flag indicating whether to include objects in the top folder.
#' @param subfolder_names A string indicating whether to add columns indicating subfolder names.
#' @export
load_data_recursive <- function(x, sub = get_sub(), main = get_main(), top = TRUE, subfolder_names = FALSE) {
  load_rds_recursive(x, class = "data", sub = sub, main = main, top = top, subfolder_names = subfolder_names)
}

#' Load Object Recursive
#'
#' @inheritParams save_object
#' @param top A flag indicating whether to include objects in the top folder.
#' @export
load_object_recursive <- function(x, sub = get_sub(), top = TRUE, main = get_main()) {
  load_rds_recursive(x, class = "objects", sub = sub, top = top, main = main)
}

#' Load Plot Recursive
#'
#' @inheritParams save_object
#' @param top A flag indicating whether to include objects in the top folder.
#' @export
load_plot_recursive <- function(x, sub = get_sub(), top = TRUE, main = get_main()) {
  load_rds_recursive(x, class = "plots", sub = sub, top = top, main = main)
}

#' Load Plot Data
#'
#' @inheritParams save_object
#' @param subfolder_names A string indicating whether to add columns indicating subfolder names.
#' @param top A flag indicating whether to include objects in the top folder.
#' @export
load_plot_data_recursive <- function(x, sub = get_sub(), main = get_main(),
                                     top = TRUE, subfolder_names = FALSE) {
  load_rds_recursive(x, class = "plots", sub = sub, main = main, top = top, fun = function(x) x$data,
            subfolder_names = subfolder_names)
}

#' Load Data
#'
#' @inheritParams save_object
#' @param subfolder_names A string indicating whether to add columns indicating subfolder names.
#' @param top A flag indicating whether to include objects in the top folder.
#' @export
load_table_recursive <- function(x, sub = get_sub(), main = get_main(), top = TRUE, subfolder_names = FALSE) {
  load_rds_recursive(x, class = "tables", sub = sub, main = main, top = top, subfolder_names = subfolder_names)
}

#' Load Templates
#'
#' @inheritParams save_object
#' @param subfolder_names A string indicating whether to add columns indicating subfolder names.
#' @param top A flag indicating whether to include objects in the top folder.
#' @export
load_template_recursive <- function(x, sub = get_sub(), main = get_main(), top = TRUE, subfolder_names = FALSE) {
  load_rds_recursive(x, class = "templates", sub = sub, main = main, subfolder_names = subfolder_names)
}
