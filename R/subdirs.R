subdirs <- function(class, main, sub) {
  check_string(class)
  check_string(main)
  check_string(sub)

  dir <- file_path(main, class, sub)
  list.dirs(dir, full.names = FALSE, recursive = FALSE)
}

#' List Object Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
subdirs_objects <- function(main = get_main(), sub = get_sub()) {
  subdirs("objects", main = main, sub = sub)
}

#' List Plot Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
subdirs_plots <- function(main = get_main(), sub = get_sub()) {
  subdirs("plots", main = main, sub = sub)
}

#' List Table Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
subdirs_tables <- function(main = get_main(), sub = get_sub()) {
  subdirs("tables", main = main, sub = sub)
}

#' List Template Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
subdirs_templates <- function(main = get_main(), sub = get_sub()) {
  subdirs("templates", main = main, sub = sub)
}
