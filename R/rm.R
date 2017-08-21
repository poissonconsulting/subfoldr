rm_rdss <- function(recursive, class, sub, main, ask) {
  check_flag(recursive)
  check_string(sub)
  check_string(main)
  check_flag(ask)

  dir <- file_path(main, class, sub)
  if (!dir.exists(dir)) return(0L)

  files <- list.files(dir, recursive = recursive, full.names = TRUE, include.dirs = TRUE)
  dirs <- list.dirs(dir, recursive = recursive)
  files %<>% setdiff(dirs)

  nitems <- stringr::str_replace(files, "[.][^.]+$", "") %>%
    stringr::str_replace("^_", "") %>%
    unique() %>%
    length()

  if (!recursive) {
    if (!nitems) return(0L)
    if (!ask || yesno("Delete ", nitems, " item(s) in directory '", dir, "'?")) {
      unlink(files)
      return(nitems)
    }
    return(0L)
  }

  if (!ask || yesno("Delete directory '", dir, "' and its ", nitems, " item(s)?")) {
    unlink(dir, recursive = TRUE)
    return(nitems)
  }
  0L
}

#' Remove Data
#'
#' @inheritParams save_object
#' @param recursive A flag indicating whether to recursively delete items.
#' @export
rm_datas <- function(recursive = TRUE, sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(recursive = recursive, class = "data", sub = sub, main = main, ask = ask))
}

#' Remove Data
#'
#' @inheritParams save_object
#' @param recursive A flag indicating whether to recursively delete items.
#' @export
rm_dbs <- function(recursive = TRUE, sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(recursive = recursive, class = "dbs", sub = sub, main = main, ask = ask))
}

#' Remove Objects
#'
#' @inheritParams save_object
#' @param recursive A flag indicating whether to recursively delete items.
#' @export
rm_objects <- function(recursive = TRUE, sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(recursive = recursive, class = "objects", sub = sub, main = main, ask = ask))
}

#' Remove Plots
#'
#' @inheritParams save_object
#' @param recursive A flag indicating whether to recursively delete items.
#' @export
rm_plots <- function(recursive = TRUE, sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(recursive = recursive, class = "plots", sub = sub, main = main, ask = ask))
}

#' Remove tables
#'
#' @inheritParams save_object
#' @param recursive A flag indicating whether to recursively delete items.
#' @export
rm_tables <- function(recursive = TRUE, sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(recursive = recursive, class = "tables", sub = sub, main = main, ask = ask))
}

#' Remove templates
#'
#' @inheritParams save_object
#' @param recursive A flag indicating whether to recursively delete items.
#' @export
rm_templates <- function(recursive = TRUE, sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(recursive = recursive, class = "templates", sub = sub, main = main, ask = ask))
}

#' Remove All
#'
#' @inheritParams save_object
#' @param recursive A flag indicating whether to recursively delete items.
#' @export
rm_all <- function(recursive = TRUE, sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  check_flag(ask)
  check_string(sub)
  check_string(main)
  check_flag(ask)

  if (!dir.exists(main)) return(0L)

  if (identical(sub, "")) {
    if (recursive) {
      msg <- stringr::str_c("Delete directory '", main, "' and all its items?")
    } else
      msg <- stringr::str_c("Delete all items in directory '", main, "'?")
  } else {
    if (recursive) {
      msg <- stringr::str_c("Delete '", sub,"' subdirectories of '", main, "'?")
    } else
      msg <- stringr::str_c("Delete all items in '", sub,"' subdirectories of '", main, "'?")
  }

  nitems <- 0L
  if (!ask || yesno(msg)) {

    nitems %<>%
      magrittr::add(rm_datas(recursive = recursive, sub = sub, main = main, ask = FALSE)) %>%
      magrittr::add(rm_tables(recursive = recursive, sub = sub, main = main, ask = FALSE)) %>%
      magrittr::add(rm_objects(recursive = recursive, sub = sub, main = main, ask = FALSE)) %>%
      magrittr::add(rm_plots(recursive = recursive, sub = sub, main = main, ask = FALSE)) %>%
      magrittr::add(rm_templates(recursive = recursive, sub = sub, main = main, ask = FALSE)) %>%
      magrittr::add(rm_dbs(recursive = recursive, sub = sub, main = main, ask = FALSE))

    if (identical(sub, "") && recursive)
      unlink(main, recursive = TRUE)

  }
  invisible(nitems)
}
