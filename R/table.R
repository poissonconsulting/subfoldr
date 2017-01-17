#' Save Object as .csv
#'
#' @inheritParams save_object
#' @return The object x or TRUE or FALSE is x is missing.
#' @export
save_table <- function(x, type = "", main = get_main(), sub = get_sub(),
                       x_name = NULL, ask = getOption("subfoldr.ask", TRUE)) {
  check_string(type)
  check_string(main)
  check_string(sub)
  check_flag(ask)

  if (missing(x)) {

    names <- objects(envir = calling_env())

    flag <- FALSE
    for (name in names) {
      table <- get(x = name, envir = calling_env())
      if (is.data.frame(table))
        save_table(table, type = type, main = main, sub = sub, x_name = name, ask = ask)
        flag <- TRUE
    }
    if (!flag) warning("no data.frames in calling environment")
    return(invisible(flag))
  }

  if (!is.data.frame(x)) error("x must be a data.frame")

  if (is.null(x_name)) x_name <- deparse(substitute(x))

  check_string(x_name)

  dir <- file_path(main, "tables", type, sub)

  create_dir(dir, ask)

  file <- file_path(dir, x_name) %>% str_c(".rds")

  saveRDS(x, file)

  file %<>% str_replace("[.]rds$", ".csv")

  readr::write_csv(x, file)

  invisible(x)
}

#' Load table
#'
#' @inheritParams save_table
#' @export
load_table <- function(x = NULL, type = "", main = get_main(), sub = get_sub()) {

  if (!missing(x)) {
    check_string(x)
    file <- file_path(main, "tables", type, sub, x) %>% str_c(".rds")
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
