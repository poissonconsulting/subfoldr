save_rds <- function(x, path = getOption(subfolder.rds, "output/rds"), folder = "data", x_name = NULL) {

  if (is.null(x_name)) x_name <- deparse(substitute(object))

  file <- file.path(path, folder, x_name) %>% paste0(".rds")

  saveRDS(x, file)

  invisible(x)
}

load_rds <- function(x_name = NULL, path = getOption(subfolder.rds, "output/rds"), folder = "data",
                     env = environment(), ask =  getOption(subfolder.rds, "output/rds")) {

  if (!is.null(x_name)) {
    file <- file.path(path, folder, x_name) %>% paste0(".rds")
    return(readRDS(file))
  }

  files <- list.files(path = file.path(path, folder), pattern = "[.]rds$")
  if (!length(files))
    warning("no rds objects found")
    return(invisible(FALSE))
  }
  for (file in files) {
    x_name <- base_name(file) %>% str_replace("[.]rds$", "")
    assign(x_name, load_rds(x_name), envir = env())
  }
  invisible(TRUE)
}
