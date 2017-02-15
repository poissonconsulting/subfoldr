add_full_stop <- function(x) {
  str_replace(x, "([^.]$)", "\\1.")
}

error <- function(...) {
  stop(..., call. = FALSE)
}

file_path <- function(...) {
  path <- file.path(...)
  path %<>% str_replace_all("//", "/") %>% str_replace_all("//", "/")
  path %<>% str_replace_all("/$", "")

  path
}

class_ext <- function(class) {
  switch(class,
         plots = "png",
         tables = "csv",
         templates = "txt",
         stop())
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

list_files <- function(dir) {
  files <- list.files(dir, pattern = "_[^/]+[.]RDS$", recursive = TRUE,  all.files = TRUE, full.names = TRUE)
  rds <- lapply(files, readRDS)
  rds %<>% vapply(function(x, report) x$report, TRUE)
  names_files <- files
  files %<>% str_replace(str_c("^(.*", dir, ")(.*)([.]RDS$)"), "\\2")
  files %<>% str_replace("^(/)(.*)(_)([^_]+)$", "\\2\\4")
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

order_heading <- function(sub_row, heading, locale) {
  order <- rep(0, length(sub_row))

  for (h in names(heading)) {
    match <- str_detect(sub_row, str_c("^", h, "$")) & order == 0
    if (any(match)) {
      order[match] <- max(order) + 1
    }
  }
  names <- sub_row[order == 0] %>% unique() %>% str_sort(locale = locale)

  for (h in names) {
    match <- str_detect(sub_row, str_c("^", h, "$")) & order == 0
    if (any(match)) {
      order[match] <- max(order) + 1
    }
  }
  stopifnot(max(order) < 10^6)
  order %<>% str_pad(width = 6, pad = 0)
}

order_headings <- function(subs_matrix, headings, locale) {
  stopifnot(length(headings) <= nrow(subs_matrix))

  if (length(headings) < nrow(subs_matrix))
    headings %<>% c(lapply(1:(nrow(subs_matrix) - length(headings)), function(x) character(0)))

  for (i in seq_along(headings)) {
    subs_matrix[i,] %<>% order_heading(headings[[i]], locale)
  }

  subs_matrix %<>% plyr::alply(2, str_c, collapse = "-") %>% unlist()
  order(subs_matrix)
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

header <- function(nheader, header1) {
  str_c(rep("#", header1 + nheader - 1), collapse = "")
}

transfer_files <- function(transfers) {
  for (i in seq_along(transfers)) {
    if (!dir.exists(dirname(names(transfers)[i]))) dir.create(dirname(names(transfers)[i]), recursive = TRUE)
    file.copy(from = transfers[i], to = names(transfers)[i], overwrite = TRUE)
  }
}

last_one <- function(x) {
  wch <- which(!str_detect(x, "^$"))
  wch[length(wch)]
}

set_headers <- function(subs_matrix, nheaders, header1, locale = locale) {
  subs_matrix %<>% t()
  if (nheaders == 0) return(rep("", nrow(subs_matrix)))

  org <- subs_matrix

  last <- apply(subs_matrix, 1, last_one)

  for (i in 1:nheaders) {
    subs_matrix[1,i] %<>% str_c(header(i, header1), ., sep = " ")
    if (nrow(subs_matrix) > 1) {
      for (j in 2:nrow(subs_matrix)) {
        if (subs_matrix[j,i] == org[j - 1, i]) {
          subs_matrix[j,i] <- ""
        } else {
          subs_matrix[j,i] %<>% str_c(header(i, header1), ., sep = " ")
        }
      }
    }
  }
  if (ncol(subs_matrix) > nheaders)
    subs_matrix[,(nheaders + 1):ncol(subs_matrix)] <- ""

  for (i in seq_along(last)) subs_matrix[i,last[i]] <- ""

  subs_matrix %<>% plyr::alply(1, str_c, collapse = "\n") %>% unlist()
  subs_matrix %<>% vapply(str_to_title, "", locale = locale)
  subs_matrix %<>% str_replace_all("\n+", "\n")
  subs_matrix %<>% str_replace("^\n", "") %>% str_replace("\n$", "")
  subs_matrix
}
