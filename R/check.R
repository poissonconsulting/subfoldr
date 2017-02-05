check_md_args <- function(headings, drop, report, main, sub, class, ask) {
  check_flag(report)
  check_string(main)
  check_string(sub)
  check_string(class)
  check_flag(ask)
  if (!is.list(headings)) error("headings must be a list")
  if (!is.list(drop)) error("drop must be a list")
  if (!all(vapply(headings, is.character, TRUE)))
    error("headings must be a list of character vectors")
  if (!all(vapply(drop, is.character, TRUE)))
    error("drop must be a list of character vectors")
  if (!all(vapply(headings, function(x) !length(x) || !is.null(names), TRUE)))
    error("headings must be a list of named character vectors")

  dir <- file.path(main, class, sub)

  files <- list_files(dir, report)

  if (!length(files)) return(TRUE)

  nsub <- max(nsubs(files))

  if (nsub < length(headings)) error("there are more vectors in headings than subfolders")
  if (nsub < length(drop)) error("there are more vectors in headings than subfolders")
  TRUE
}
