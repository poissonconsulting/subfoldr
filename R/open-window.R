#' Open a new graphics window.
#'
#' @param width A number indicating the width in inches.
#' @param height A number indicating the height in inches.
#' @export
open_window <- function(width = 6, height = width) {
  fun <- switch(Sys.info()["sysname"],
                Windows = grDevices::windows,
                Darwin = grDevices::quartz,
                grDevices::x11)

  fun(width = width, height = height)
}
