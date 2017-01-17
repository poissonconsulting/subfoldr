error <- function(...) {
  stop(..., call. = FALSE)
}

#' Get sub
#'
#' @return A string of the current sub.
#' @export
#'
#' @examples
#' get_sub()
get_sub <- function() {
  sub <- getOption("subfoldr.sub", "")
  sub
}

#' Set sub
#'
#' @param ... One or more strings
#' @return A string of the old sub.
#' @export
set_sub <- function(...) {
  sub <- file_path(...)
  check_string(sub)
  old_sub <- get_sub()
  options(subfoldr.sub = sub)
  old_sub
}

#' Reset sub
#'
#' @return A string of the old sub.
#' @export
reset_sub <- function() {
  old_sub <- get_sub()
  options(subfoldr.sub = "")
  old_sub
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
#' @return A string of the old main.
#' @export
set_main <- function(...) {
  main <- file_path(...)
  check_string(main)
  old_main <- get_main()
  options(subfoldr.main = main)
  old_main
}

#' Reset Main
#'
#' @return A string of the old main.
#' @export
reset_main <- function() {
  old_main <- get_main()
  options(subfoldr.main = "output")
  old_main
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
  path
}

create_dir <- function(dir, ask) {
  if (!dir.exists(dir)) {
    if (ask && !yesno("Create directory '", dir, "'?"))
      return(invisible(x))
    dir.create(dir, recursive = TRUE)
  }
  dir
}

#' Calling Environment
#' @export
#' @examples
#' calling_env()
calling_env <- function() {
  parent.frame(n = 2)
}

load_rds <- function(x, class, type = "", main = get_main(), sub = get_sub(), env = calling_env()) {
  check_string(class)
  check_string(type)
  check_string(main)
  check_string(sub)

  if (!missing(x)) {
    check_string(x)
    file <- file_path(main, class, type, sub, x) %>% str_c(".rds")
    if (!file.exists(file)) error("file '", file, "' does not exist")
    return(readRDS(file))
  }

  files <- list.files(path = file_path(main, class, type, sub), pattern = "[.]rds$")
  if (!length(files)) {
    warning("no .rds objects found")
    return(invisible(FALSE))
  }

  for (file in files) {
    x <- basename(file) %>% str_replace("[.]rds$", "")
    assign(x, load_rds(x, class = class, type = type, main = main, sub = sub), envir = env)
  }
  invisible(TRUE)
}
