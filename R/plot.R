#' Save Object as .png
#'
#' @inheritParams save_object
#' @inheritParams ggplot2::ggsave
#' @param x A string of the plot name.
#' @param height A number indicating the height of the plot in inches.
#' @param caption A string of the figure caption.
#' @export
save_plot <- function(x, type = "", main = get_main(), sub = get_sub(),
                      width = NA, height = NA, dpi = 300, caption = "",
                      ask = getOption("subfoldr.ask", TRUE),
                      plot = ggplot2::last_plot()) {

  check_string(x)
  check_string(type)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)

  if (is.null(plot)) error("plot is NULL")

  save_rds(plot$data, "plots", type = type, main = main, sub = sub, x_name = x, ask = ask)

  obj <- list(plot = plot, width = width, height = height, dpi = dpi, caption = caption)
  file <- file_path(main, "plots", type, sub, str_c(".", x)) %>% str_c(".rds")
  saveRDS(obj, file = file)

  file <- file_path(main, "plots", type, sub, x) %>% str_c(".csv")
  readr::write_csv(plot$data, file = file)

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
load_plot <- function(x, type = "", main = get_main(), sub = get_sub(), data = FALSE, env = calling_env()) {
  check_flag(data)

  if (data) return(x, class = "plots", type = type, main = main, sub = sub, env = env)

  check_string(x)

  file <- file_path(main, "plots", type, sub, str_c(".", x)) %>% str_c(".rds")
  if (!file.exists(file)) error("file '", file, "' does not exist")

  x <- readRDS(file)

  x$plot
}
