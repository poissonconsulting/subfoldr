subdirs <- function(class, main, sub) {
  check_string(class)
  check_string(main)
  check_string(sub)

  dir <- file_path(main, class, sub)
  list.dirs(dir, full.names = FALSE, recursive = FALSE)
}

#' List Data Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
subdirs_data <- function(sub = get_sub(), main = get_main()) {
  subdirs("data", main = main, sub = sub)
}

#' List Object Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
subdirs_objects <- function(sub = get_sub(), main = get_main()) {
  subdirs("objects", main = main, sub = sub)
}

#' List Plot Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
subdirs_plots <- function(sub = get_sub(), main = get_main()) {
  subdirs("plots", main = main, sub = sub)
}

#' List Table Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
subdirs_tables <- function(sub = get_sub(), main = get_main()) {
  subdirs("tables", main = main, sub = sub)
}

#' List Template Sub Directories
#'
#' @inheritParams save_object
#' @return A character vector of the names of the directories in the sub folder.
#' @export
subdirs_templates <- function(sub = get_sub(), main = get_main()) {
  subdirs("templates", main = main, sub = sub)
}
