#' Save Object as .png
#'
#' @inheritParams save_object
#' @inheritParams ggplot2::ggsave
#' @param x A string of the plot name.
#' @param height A number indicating the height of the plot in inches.
#' @param caption A string of the figure caption.
#' @export
save_plot <- function(x, type = get_type(), main = get_main(), sub = get_sub(),
                      width = NA_real_, height = NA_real_, dpi = 300, caption = "",
                      ask = getOption("subfoldr.ask", TRUE),
                      plot = ggplot2::last_plot()) {

  check_string(x)
  check_string(type)
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

  save_rds(plot$data, "plots", type = type, main = main, sub = sub, x_name = x, ask = ask)

  obj <- list(plot = plot, width = width, height = height, dpi = dpi, caption = caption)
  file <- file_path(main, "plots", type, sub, str_c(".", x)) %>% str_c(".RDS")
  saveRDS(obj, file = file)

  file <- file_path(main, "plots", type, sub, x) %>% str_c(".csv")
  readr::write_csv(plot$data, path = file)

  file %<>% str_replace("[.]csv$", ".png")
  ggplot2::ggsave(file, plot = plot, width = width, height = height, dpi = dpi)

  invisible(x)
}

#' Load table
#'
#' @inheritParams save_object
#' @param data A flag indicating whether to load the plot data.
#' @param env The environment to load the objects into if data = TRUE and  x is missing.
#' @export
load_plot <- function(x, type = get_type(), main = get_main(), sub = get_sub(), data = FALSE, env = calling_env()) {
  check_flag(data)

  if (data) return(load_rds(x, class = "plots", type = type, main = main, sub = sub, env = env))

  check_string(x)

  file <- file_path(main, "plots", type, sub, str_c(".", x)) %>% str_c(".RDS")
  if (!file.exists(file)) error("file '", file, "' does not exist")

  x <- readRDS(file)

  x$plot
}
