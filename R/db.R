#' Opens an connection to an sqlite database
#'
#' Currently an sqlite3 database
#'
#' @inheritParams save_object
#' @param x A string of the name of the db to create (without the file extension).
#' @param new A flag indicating whether to enforce connection to an existing database (FALSE) or connection to a new database (TRUE).
#' @param foreign_keys A flag indicating whether to switch foreign keys on.
#' @param ... Additional arguments passed to pdf
#' @export
open_db <- function(x, new = NA, foreign_keys = TRUE, sub = get_sub(), main = get_main(),
                    ask = getOption("subfoldr.ask", TRUE), ...) {
  check_string(x)
  check_string(main)
  check_string(sub)
  check_vector(new, c(TRUE, NA), length = 1)
  check_flag(foreign_keys)

  file <- file_path(main, "dbs", sub, x) %>% str_c(".sqlite")

  if (identical(new, FALSE) && !file.exists(file))
    error("database `", file, "` does not exist")

  if (identical(new, TRUE) && file.exists(file)) {
      if (ask && !yesno("Delete database '", file, "'?")) return(FALSE)
        file.remove(file)
  }
  create_dir(dirname(file), ask = ask)
  db <- DBI::dbConnect(RSQLite::SQLite(), file)
  if (foreign_keys) DBI::dbGetQuery(db, "PRAGMA foreign_keys = ON;")
  db
}

#' Close connection to a database
#'
#' A wrapper on DBI::dbDisconnect
#' @param db The connection to close.
#' @export
close_db <- function(db) DBI::dbDisconnect(db)
