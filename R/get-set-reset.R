#' Increase Table Number
#'
#' @return A string of the new table number.
#' @export
incr_table_number <- function() {
  set_table_number(get_table_number() + 1L)
}

#' Increase Template Number
#'
#' @return A string of the new template number.
#' @export
incr_template_number <- function() {
  set_template_number(get_template_number() + 1L)
}

#' Increase Plot Number
#'
#' @return A string of the new plot number.
#' @export
incr_plot_number <- function() {
  set_plot_number(get_plot_number() + 1L)
}

#' Get Table Number
#'
#' @return A string of the last table number.
#' @export
get_table_number <- function() {
  getOption("subfoldr.tab_num", 0L)
}

#' Set Table Number
#'
#' @param x A count.
#' @return A string of the new table number.
#' @export
set_table_number <- function(x) {
  check_count(x)
  options(subfoldr.tab_num = x)
  invisible(x)
}

#' Reset Table Number
#'
#' @return A string of the new table number.
#' @export
reset_table_number <- function() {
  options(subfoldr.tab_num = 0L)
  invisible(0L)
}

#' Get Template Number
#'
#' @return A string of the last template number.
#' @export
get_template_number <- function() {
  getOption("subfoldr.tem_num", 0L)
}

#' Set Template Number
#'
#' @param x A count.
#' @return A string of the new template number.
#' @export
set_template_number <- function(x) {
  check_count(x)
  options(subfoldr.tem_num = x)
  invisible(x)
}

#' Reset Template Number
#'
#' @return A string of the new template number.
#' @export
reset_template_number <- function() {
  options(subfoldr.tem_num = 0L)
  invisible(0L)
}

#' Get Plot Number
#'
#' @return A string of the last plot number.
#' @export
get_plot_number <- function() {
  sub <- getOption("subfoldr.plot_num", 0L)
  sub
}

#' Set Plot Number
#'
#' @param x A count.
#' @return A string of the new plot number.
#' @export
set_plot_number <- function(x) {
  check_count(x)
  options(subfoldr.plot_num = x)
  invisible(x)
}

#' Reset Plot Number
#'
#' @return A string of the new plot number.
#' @export
reset_plot_number <- function() {
  options(subfoldr.plot_num = 0L)
  invisible(0L)
}

#' Get sub
#'
#' @return A string of the current sub.
#' @export
get_sub <- function() {
  getOption("subfoldr.sub", "")
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
  getOption("subfoldr.main", "output")
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
  getOption("subfoldr.report", "report")
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
  reset_plot_number()
  reset_table_number()
  reset_template_number()
  invisible(TRUE)
}
