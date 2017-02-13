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

#' Get Report
#'
#' @return A string of the report folder.
#' @export
#'
#' @examples
#' get_report()
get_report <- function() {
  report <- getOption("subfoldr.report", "report")
  report
}

#' Set Report
#'
#' @param ... One or more strings
#' @return A string of the new main.
#' @export
set_report <- function(...) {
  report <- file_path(...)
  check_string(report)
  options(subfoldr.report = report)
  invisible(report)
}

#' Reset Report
#'
#' @return A string of the new report
#' @export
reset_report <- function() {
  options(subfoldr.report = "report")
  invisible("report")
}

#' Reset All
#'
#' Resets main, sub and report.
#' @return An invisible flag indicating whether successful.
#' @export
reset_all <- function() {
  reset_main()
  reset_sub()
  reset_report()
  invisible(TRUE)
}
