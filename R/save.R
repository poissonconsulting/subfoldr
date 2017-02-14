save_rds <- function(x, class, main, sub, x_name, ask) {
  check_string(class)
  check_string(main)
  check_string(sub)
  check_flag(ask)
  check_string(x_name)

  dir <- file_path(main, class, sub)

  create_dir(dir, ask)

  file <- file_path(dir, x_name) %>% str_c(".rds")

  saveRDS(x, file)

  invisible(x)
}

#' Save Object
#'
#' @param x The object to save. If missing saves all objects in calling env.
#' @param main A string of the main subfolder.
#' @param sub A string of the path to the directory to save the object.
#' @param is A function returning TRUE or FALSE to indicate whether to save particular objects.
#' Ignored unless x is missing.
#' @param x_name An optional string of the name to use.
#' @param ask A string indicating whether to ask before creating a sub directory.
#' @return The object x or TRUE or FALSE is x is missing.
#' @export
save_object <- function(x, x_name = NULL, main = get_main(), sub = get_sub(),
                        is = is.data.frame,
                        ask = getOption("subfoldr.ask", TRUE)) {
  check_string(main)
  check_string(sub)
  check_flag(ask)
  if (!is.function(is)) error("is must be a function")

  if (missing(x)) {
    names <- objects(envir = calling_env())
    flag <- FALSE
    for (x_name in names) {
      x <- get(x = x_name, envir = calling_env())
      if (is(x))
        save_rds(x, "objects", main = main, sub = sub, x_name = x_name, ask = ask)
      flag <- TRUE
    }
    if (!flag) warning("no suitable 'objects' in calling environment")
    return(invisible(flag))
  }

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)

  save_rds(x, "objects", main = main, sub = sub, x_name = x_name, ask = ask)
}

#' Save Object as .png
#'
#' @inheritParams save_object
#' @inheritParams ggplot2::ggsave
#' @param x A string of the plot name.
#' @param caption A string of the figure caption.
#' @param report A flag indicating to include the plot in reports.
#' @param height A number indicating the height of the plot in inches.
#' @export
save_plot <- function(x, caption = "", report = TRUE,
                      main = get_main(), sub = get_sub(),
                      width = NA_real_, height = NA_real_, dpi = 300,
                      ask = getOption("subfoldr.ask", TRUE),
                      plot = ggplot2::last_plot()) {

  check_string(x)
  check_flag(report)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)
  check_scalar(width, c(1, NA))
  check_scalar(height, c(1, NA))

  if (is.null(plot)) error("plot is NULL")

  if (is.na(width)) width <- height
  if (is.na(height)) height <- width

  if (is.na(width)) {
    if (!length(grDevices::dev.list())) {
      width = 6
      height = 6
    } else {
      dim <- grDevices::dev.size(units = "in")
      width <- dim[1]
      height <- dim[2]
    }
  }

  save_rds(plot, "plots", main = main, sub = sub, x_name = x, ask = ask)

  obj <- list(width = width, height = height, dpi = dpi, caption = caption, report = report)
  file <- file_path(main, "plots", sub, str_c("_", x)) %>% str_c(".RDS")
  saveRDS(obj, file = file)

  file <- file_path(main, "plots", sub, x) %>% str_c(".csv")
  readr::write_csv(plot$data, path = file)

  file %<>% str_replace("[.]csv$", ".png")
  ggplot2::ggsave(file, plot = plot, width = width, height = height, dpi = dpi)

  invisible(x)
}

#' Save Object as .csv
#'
#' @inheritParams save_object
#' @param caption A string of the figure caption.
#' @param report A flag indicating to include the plot in reports.
#' @return The object x.
#' @export
save_table <- function(x, x_name = NULL, caption = "", report = TRUE,
                       main = get_main(), sub = get_sub(),
                       ask = getOption("subfoldr.ask", TRUE)) {

  check_flag(report)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)

  if (!is.data.frame(x)) error(x_name, " must be a data.frame")

  save_rds(x, "tables", main = main, sub = sub, x_name = x_name, ask = ask)

  obj <- list(caption = caption, report = report)
  file <- file_path(main, "tables", sub, str_c("_", x_name)) %>% str_c(".RDS")
  saveRDS(obj, file = file)

  file <- file_path(main, "tables", sub, x_name) %>% str_c(".csv")
  readr::write_csv(x, path = file)

  invisible(x)
}

#' Save string as .txt
#'
#' @inheritParams save_object
#' @param caption A string of the template caption.
#' @param report A flag indicating to include the plot in reports.
#' @return The object x.
#' @export
save_template <- function(x, x_name = NULL, caption = "", report = TRUE,
                          main = get_main(), sub = get_sub(),
                          ask = getOption("subfoldr.ask", TRUE)) {

  check_flag(report)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x, x_name = x_name)
  check_string(x_name)

  save_rds(x, "templates", main = main, sub = sub, x_name = x_name, ask = ask)

  obj <- list(caption = caption, report = report)
  file <- file_path(main, "templates", sub, str_c("_", x_name)) %>% str_c(".RDS")
  saveRDS(obj, file = file)

  file <- file_path(main, "templates", sub, x_name) %>% str_c(".txt")
  cat(x, file = file)

  invisible(x)
}
