#' Save Object as .png
#'
#' @inheritParams save_object
#' @inheritParams ggplot2::ggsave
#' @param x A string of the plot name.
#' @param caption A string of the figure caption.
#' @export
save_plot <- function(x, type = "", main = get_main(), sub = get_sub(),
                      width = 6, height = 6, dpi = 300, caption = "",
                      ask = getOption("subfoldr.ask", TRUE),
                      plot = ggplot2::last_plot()) {
  check_string(type)
  check_string(main)
  check_string(sub)
  check_string(caption)
  check_flag(ask)

  if (is.null(plot)) error("plot is NULL")

  dir <- file_path(main, "plots", type, sub)

  create_dir(dir, ask)

  file <- file_path(dir, x_name) %>% str_c(".rds")

  obj <- list(plot = plot, width = width, height = height, caption = caption)
  saveRDS(obj, file = file)

  data <- obj$plot$data

  file %<>% str_replace("[.]rds$", ".csv")
  readr::write_csv(data, file = file)

  file %<>% str_replace("[.]rds$", ".png")
  ggplot2::ggsave(file, plot = plot, width = width, height = height, dpi = dpi)

  invisible(x)
}

#' Load table
#'
#' @inheritParams save_table
#' @export
load_table <- function(x, type = "", main = get_main(), sub = get_sub()) {

  if (!is.null(x_name)) {
    file <- file_path(main, "tables", type, sub, x_name) %>% str_c(".rds")
    if (!file.exists(file)) error("file '", file, "' does not exist")
    return(readRDS(file))
  }

  files <- list.files(path = file_path(main, "tables", type, sub), pattern = "[.]rds$")
  if (!length(files)) {
    warning("no .rds tables found")
    return(invisible(FALSE))
  }

  for (file in files) {
    x_name <- basename(file) %>% str_replace("[.]rds$", "")
    assign(x_name, load_object(x_name, type = type, main = main, sub = sub), envir = calling_env())
  }
  invisible(TRUE)
}
