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
