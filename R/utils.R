error <- function(...) {
  stop(..., call. = FALSE)
}

file_path <- function(...) {
  path <- file.path(...)
  path %<>% str_replace_all("//", "/") %>% str_replace_all("//", "/")
  path %<>% str_replace_all("/$", "")

  path
}

create_dir <- function(dir, ask) {
  if (!dir.exists(dir)) {
    if (ask && !yesno("Create directory '", dir, "'?"))
      return(FALSE)
    dir.create(dir, recursive = TRUE)
  }
  TRUE
}

#' Calling Environment
#' @export
#' @examples
#' calling_env()
calling_env <- function() {
  parent.frame(n = 2)
}

sub_names <- function(x) {
  str_split(x, "/")
}

nsubs <- function(x) {
  x %<>% sub_names()
  vapply(x, length, 1L)
}

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

list_files <- function(dir, report) {
  files <- list.files(dir, pattern = "[.][^/]+[.]RDS$", recursive = TRUE,  all.files = TRUE, full.names = TRUE)
  rds <- lapply(files, readRDS)
  rds %<>% vapply(function(x, report) x$report == report, TRUE, report)
  names_files <- files
  files %<>% str_replace(str_c("^(.*", dir, ")(.*)([.]RDS$)"), "\\2")
  files %<>% str_replace("^(/)(.*)([.])([^.]+)$", "\\2\\4")
  names(files) <- names_files
  files[rds]
}

subs_matrix <- function(x) {
  x %<>% str_split("/", simplify = TRUE)
  x %<>% t()
  x
}

drop_rows <- function(subs_matrix, drop) {
  stopifnot(length(drop) <= nrow(subs_matrix))

  bol <- rep(FALSE, ncol(subs_matrix))
  for (i in seq_along(drop)) {
    bol <- bol | subs_matrix[i,,drop = TRUE] %in% drop[[i]]
  }
  bol
}

rename_heading <- function(sub_row, heading) {
  for (i in seq_along(heading)) {
    sub_row %<>% str_replace(str_c("^", names(heading[i]), "$"), heading[i])
  }
  sub_row
}

rename_headings <- function(subs_matrix, headings) {
  stopifnot(length(headings) <= nrow(subs_matrix))
  for (i in seq_along(headings)) {
    subs_matrix[i,] %<>% rename_heading(headings[[i]])
  }
  subs_matrix
}

make_headers <- function(headers) {
  headers <- seq(headers[1], headers[2])
  headers %<>% vapply(function(x) stringr::str_c(rep("#", x), collapse = ""), "")
  headers
}

set_headers <- function(subs_matrix, headers) {
  headers %<>% make_headers()

  for (i in 1:length(headers)) {
    subs_matrix[1,headers[i]] %<>% str_c(headers[i], ., collapse = " ")
  }

  if (nrow(subs_matrix) == 1) return(subs_matrix)

  for (i in 2:nrow(subs_matrix)) {
    for (j in 1:length(headers)) {
      if (!identical(subs_matrix[i,j], subs_matrix[i - 1, j]))
        subs_matrix[i,headers[j]] %<>% str_c(headers[j], ., collapse = " ")
    }
  }
  subs_matrix
}
