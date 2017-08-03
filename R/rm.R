rm_rdss <- function(class, sub, main, ask) {
  dir <- file_path(main, class, sub)
  if (!dir.exists(dir)) return(FALSE)

  files <- list.files(dir, full.names = TRUE)
  dirs <- list.dirs(dir, recursive = FALSE)
  files %<>% setdiff(dirs)
  if (!length(files)) return(FALSE)

  if (!ask || yesno("Delete ", length(files), "file(s) in directory '", dir, "'?"))
    file.remove(files)
  TRUE
}

#' Remove Data
#'
#' @inheritParams save_object
#' @export
rm_datas <- function(sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(class = "data", sub = sub, main = main, ask = ask))
}

#' Remove Objects
#'
#' @inheritParams save_object
#' @export
rm_objects <- function(sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(class = "objects", sub = sub, main = main, ask = ask))
}

#' Remove Plots
#'
#' @inheritParams save_object
#' @export
rm_plots <- function(sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(class = "plots", sub = sub, main = main, ask = ask))
}

#' Remove tables
#'
#' @inheritParams save_object
#' @export
rm_tables <- function(sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(class = "tables", sub = sub, main = main, ask = ask))
}

#' Load template
#'
#' @inheritParams save_object
#' @export
rm_templates <- function(sub = get_sub(), main = get_main(), ask = getOption("subfoldr.ask", TRUE)) {
  invisible(rm_rdss(class = "templates", sub = sub, main = main, ask = ask))
}
