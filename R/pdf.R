#' Open a pdf
#'
#' @inheritParams save_object
#' @param x A string of the name of the file to create.
#' @param width A number indicating the width in inches.
#' @param height A number indicating the height in inches.
#' @param ... Additional arguments passed to pdf
#' @export
open_pdf <- function(x, width = 6, height = width, sub = get_sub(), main = get_main(),
                     ask = getOption("subfoldr.ask", TRUE), ...) {
  check_string(x)
  check_string(main)
  check_string(sub)

  file <- file_path(main, "pdfs", sub, x) %>% str_c(".pdf")

  create_dir(dirname(file), ask)

  grDevices::pdf(file = file, width = width, height = height, ...)
}

#' Close the open pdf a pdf
#'
#' The function is just a wrapper on dev.off()
#' @export
close_pdf <- function() grDevices::dev.off()
