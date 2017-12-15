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

#' Save Data
#'
#' @param x The data frame to save. If missing saves all data frames in calling env.
#' @param main A string of the main subfolder.
#' @param sub A string of the path to the directory to save the object.
#' @param x_name An optional string of the name to use.
#' @param ask A string indicating whether to ask before creating a sub directory.
#' @return The object x or TRUE or FALSE is x is missing.
#' @export
save_data <- function(x, x_name = NULL, sub = get_sub(), main = get_main(),
                      ask = getOption("subfoldr.ask", TRUE)) {
  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)
  check_filename(x_name)

  check_data(x, x_name = deparse(substitute(x)))

  check_string(main)
  check_string(sub)
  check_flag(ask)

  save_rds(x, "data", main = main, sub = sub, x_name = x_name, ask = ask)
}

#' Save Object
#'
#' @param x The object to save.
#' @param main A string of the main subfolder.
#' @param sub A string of the path to the directory to save the object.
#' @param x_name An optional string of the name to use.
#' @param ask A string indicating whether to ask before creating a sub directory.
#' @return The object x or TRUE or FALSE is x is missing.
#' @export
save_object <- function(x, x_name = NULL, sub = get_sub(), main = get_main(),
                        ask = getOption("subfoldr.ask", TRUE)) {
  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)
  check_filename(x_name)

  check_string(main)
  check_string(sub)
  check_flag(ask)


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
#' @param csv A flag indicating whether to save a csv of the plot data.
#' @export
save_plot <- function(x, sub = get_sub(), main = get_main(),
                      caption = "", report = TRUE,
                      width = NA_real_, height = NA_real_, dpi = 300,
                      csv = TRUE,
                      ask = getOption("subfoldr.ask", TRUE),
                      plot = ggplot2::last_plot()) {

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)
  check_filename(x_name)

  check_flag(report)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)
  check_length1(width, c(1, NA))
  check_length1(height, c(1, NA))
  check_flag(csv)

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
  if (csv) readr::write_csv(plot$data, path = file)

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
save_table <- function(x, x_name = NULL, sub = get_sub(), main = get_main(),
                       caption = "", report = TRUE,
                       ask = getOption("subfoldr.ask", TRUE)) {

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)
  check_filename(x_name)

  check_data(x, x_name = deparse(substitute(x)))

  check_flag(report)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)

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
save_template <- function(x, x_name = NULL, sub = get_sub(), main = get_main(),
                          caption = "", report = TRUE,
                          ask = getOption("subfoldr.ask", TRUE)) {

  if (is.null(x_name)) x_name <- deparse(substitute(x))
  check_string(x_name)
  check_filename(x_name)

  check_string(x, x_name = deparse(substitute(x)))

  check_flag(report)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)

  save_rds(x, "templates", main = main, sub = sub, x_name = x_name, ask = ask)

  obj <- list(caption = caption, report = report)
  file <- file_path(main, "templates", sub, str_c("_", x_name)) %>% str_c(".RDS")
  saveRDS(obj, file = file)

  file <- file_path(main, "templates", sub, x_name) %>% str_c(".txt")
  cat(x, file = file)

  invisible(x)
}

#' Save data frames in the environ
#'
#' @param main A string of the main subfolder.
#' @param sub A string of the path to the directory to save the object.
#' @param ask A string indicating whether to ask before creating a sub directory.
#' @param env The environment to get the data from.
#' @return TRUE or FALSE
#' @export
save_datas <- function(sub = get_sub(), main = get_main(), env = calling_env(),
                       ask = getOption("subfoldr.ask", TRUE)) {
  check_string(main)
  check_string(sub)
  check_flag(ask)

  names <- objects(envir = env)
  flag <- FALSE
  for (x_name in names) {
    check_filename(x_name)
    x <- get(x = x_name, envir = env)
    if (is.data.frame(x))
      save_rds(x, "data", main = main, sub = sub, x_name = x_name, ask = ask)
    flag <- TRUE
  }
  if (!flag) warning("no objects inheriting from data.frame in calling environment")
  invisible(flag)
}
