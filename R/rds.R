#' Save Object as .rds
#'
#' @param x The object to save
#' @param type A string of the type of x.
#' @param main A string of the main subfolder.
#' @param sub A string of the path to the directory to save the object.
#' @param x_name An optional string of the name to use.
#' @param ask A string indicating whether to ask befor creating a sub directory.
#' @return The object x.
#' @export
save_rds <- function(x, type = "", main = get_main(), sub = get_sub(),
                     x_name = NULL, ask = getOption("subfoldr.ask", TRUE)) {
  check_string(type)
  check_string(main)
  check_string(sub)

  if (is.null(x_name)) x_name <- deparse(substitute(object))

  check_string(x_name)
  check_flag(ask)

  dir <- file_path(main, "rds", type, sub)

  if (!dir.exists(dir)) {
    if (ask && !yesno("Create directory '", dir, "'?"))
      return(invisible(x))
    dir.create(dir)
  }

  file <- file_path(dir, x_name) %>% str_c(".rds")

  saveRDS(x, file)

  invisible(x)
}

load_rds <- function(x_name = NULL, type = "", main = get_main(), sub = get_sub(),
                      env = environment()) {

  if (!is.null(x_name)) {

    file <- file_path(main, "rds", type, sub, x_name) %>% str_c(".rds")
    if (!file.exists(file)) error("file '", file, "' does not exist")
    return(readRDS(file))
  }

  files <- list.files(path = file_path(main, "rds", type, sub), pattern = "[.]rds$")
  if (!length(files)) warning("no .rds objects found")
    return(invisible(FALSE))

  for (file in files) {
    x_name <- basename(file) %>% str_replace("[.]rds$", "")
    assign(x_name, load_rds(x_name, type = type, main = main, sub = sub), envir = env())
  }
  invisible(TRUE)
}
