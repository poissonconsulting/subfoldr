rds_exists <- function(x, class, main, sub) {
  check_string(x)
  file <- file_path(main, class, sub, x) %>% str_c(".rds")
  file.exists(file)
}

#' Template exists
#'
#' Test whether an template named x exists.
#'
#' @inheritParams save_object
#' @export
template_exists <- function(x, main = get_main(), sub = get_sub()) {
  rds_exists(x = x, main = main, sub = sub, class = "templates")
}

#' Object exists
#'
#' Test whether an object named x exists.
#'
#' @inheritParams save_object
#' @export
object_exists <- function(x, main = get_main(), sub = get_sub()) {
  rds_exists(x = x, main = main, sub = sub, class = "objects")
}

#' Table exists
#'
#' Test whether a table named x exists.
#'
#' @inheritParams save_object
#' @export
table_exists <- function(x, main = get_main(), sub = get_sub()) {
  rds_exists(x = x, main = main, sub = sub, class = "tables")
}

#' Plot exists
#'
#' Test whether a plot named x exists.
#'
#' @inheritParams save_object
#' @export
plot_exists <- function(x, main = get_main(), sub = get_sub()) {
  rds_exists(x = x, main = main, sub = sub, class = "plots")
}
