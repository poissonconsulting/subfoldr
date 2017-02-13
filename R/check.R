check_md_args <- function(headings, drop, main, sub, report, headers, locale, class) {
  check_string(main)
  check_string(sub)
  check_string(class)
  check_string(report)
  check_string(locale)
  check_vector(headers, values = c(1L, 6L), min_length = 2, max_length = 2)
  if (headers[1] > headers[2]) error("headers[1] must not be greater than headers[2]")

  stopifnot(class %in% c("templates", "tables", "objects", "plots"))

  if (!is.list(headings)) error("headings must be a list")
  if (!is.list(drop)) error("drop must be a list")
  if (!all(vapply(headings, is.character, TRUE)))
    error("headings must be a list of character vectors")
  if (!all(vapply(drop, is.character, TRUE)))
    error("drop must be a list of character vectors")
  if (!all(vapply(headings, function(x) !length(x) || !is.null(names), TRUE)))
    error("headings must be a list of named character vectors")

  dir <- file.path(main, class, sub)

  files <- list_files(dir, report = TRUE)

  if (!length(files)) return(TRUE)

  nsub <- max(nsubs(files))

  if (nsub < diff(headers)) error("there are more headers than subfolders")
  if (nsub < length(headings)) error("there are more vectors in headings than subfolders")
  if (nsub < length(drop)) error("there are more vectors in headings than subfolders")
  TRUE
}
