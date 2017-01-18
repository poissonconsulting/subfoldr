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
  options(subfoldr.sub = sub)
  invisible(sub)
}

#' Add sub
#'
#' @param ... One or more strings
#' @return A string of the old sub.
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
#' @return A string of the old sub.
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
#' @return A string of the old main.
#' @export
set_main <- function(...) {
  main <- file_path(...)
  check_string(main)
  options(subfoldr.main = main)
  invisible(main)
}

#' Reset Main
#'
#' @return A string of the old main.
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

save_rds <- function(x, class, type, main, sub, x_name, ask) {
  check_string(class)
  check_string(type)
  check_string(main)
  check_string(sub)
  check_flag(ask)
  check_string(x_name)

  dir <- file_path(main, class, type, sub)

  create_dir(dir, ask)

  file <- file_path(dir, x_name) %>% str_c(".rds")

  saveRDS(x, file)

  invisible(x)
}

load_rds <- function(x, class, type, main, sub, env) {
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
    assign(x, load_rds(x, class = class, type = type, main = main, sub = sub, env = env), envir = env)
  }
  invisible(TRUE)
}
