error <- function(...) {
  stop(..., call. = FALSE)
}

#' Get sub
#'
#' @return A string of the current sub.
#' @export
get_sub <- function() {
  sub <- getOption("subfoldr.sub", "")
  sub
}

#' Set sub
#'
#' @param ... One or more strings
#' @return A string of the new sub.
#' @export
set_sub <- function(...) {
  sub <- file_path(...)
  check_string(sub)
  options(subfoldr.sub = sub)
  invisible(sub)
}

#' Add sub
#'
#' @param ... One or more strings
#' @return A string of the new sub.
#' @export
add_sub <- function(...) {
  sub <- file_path(...)
  check_string(sub)
  old_sub <- get_sub()
  sub %<>% file_path(old_sub, .)
  options(subfoldr.sub = sub)
  invisible(sub)
}

#' Reset sub
#'
#' @return A string of the new sub.
#' @export
reset_sub <- function() {
  options(subfoldr.sub = "")
  invisible("")
}

#' Get Main
#'
#' @return A string of the main subfolder.
#' @export
#'
#' @examples
#' get_main()
get_main <- function() {
  main <- getOption("subfoldr.main", "output")
  main
}

#' Set Main
#'
#' @param ... One or more strings
#' @return A string of the new main.
#' @export
set_main <- function(...) {
  main <- file_path(...)
  check_string(main)
  options(subfoldr.main = main)
  invisible(main)
}

#' Reset Main
#'
#' @return A string of the new main.
#' @export
reset_main <- function() {
  options(subfoldr.main = "output")
  invisible("output")
}

#' Reset All
#'
#' Resets main and sub
#' @return An invisible flag indicating whether successful.
#' @export
reset_all <- function() {
  reset_main()
  reset_sub()
  invisible(TRUE)
}

file_path <- function(...) {
  path <- file.path(...)
  path %<>% str_replace_all("//", "/") %>% str_replace_all("//", "/")
  path %<>% str_replace("^/", "") %>% str_replace_all("/$", "")

  path
}

create_dir <- function(dir, ask) {
  if (!dir.exists(dir)) {
    if (ask && !yesno("Create directory '", dir, "'?"))
      return(FALSE)
    dir.create(dir, recursive = TRUE)
  }
  TRUE
}

#' Calling Environment
#' @export
#' @examples
#' calling_env()
calling_env <- function() {
  parent.frame(n = 2)
}

save_rds <- function(x, class, main, sub, x_name, ask) {
  check_string(class)
  check_string(main)
  check_string(sub)
  check_flag(ask)
  check_string(x_name)

  dir <- file_path(main, class, sub)

  create_dir(dir, ask)

  file <- file_path(dir, x_name) %>% str_c(".rds")

  saveRDS(x, file)

  invisible(x)
}

subdirs <- function(class, main, sub) {
  check_string(class)
  check_string(main)
  check_string(sub)

  dir <- file_path(main, class, sub)
  list.dirs(dir, full.names = FALSE, recursive = FALSE)
}


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

sub_names <- function(x) {
  str_split(x, "/")
}

nsubs <- function(x) {
  x %<>% sub_names()
  vapply(x, length, 1L)
}

list_files <- function(dir, report) {
  files <- list.files(dir, pattern = "[.][^/]+[.]RDS$", recursive = TRUE,  all.files = TRUE, full.names = TRUE)
  rds <- lapply(files, readRDS)
  rds %<>% vapply(function(x, report) x$report == report, TRUE, report)
  names_files <- files
  files %<>% str_replace(str_c("^(.*", dir, ")(.*)([.]RDS$)"), "\\2")
  files %<>% str_replace("^(/)(.*)([.])([^.]+)$", "\\2\\4")
  names(files) <- names_files
  files[rds]
}

subs_matrix <- function(x) {
  x %<>% str_split("/", simplify = TRUE)
  x %<>% t()
  x
}
