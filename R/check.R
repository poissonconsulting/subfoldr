check_filename <- function(x) {
  if (str_detect(x, "\"|\\[|\\]")) error(x, " is not a valid file name")
  invisible(x)
}
