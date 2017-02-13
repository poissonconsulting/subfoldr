md_files <- function(headings, drop, main, sub, report, headers, locale, class) {

  check_md_args(headings = headings, drop = drop, main = main,
                sub = sub, report = report, headers = headers, locale = locale, class = class)

  dir <- file.path(main, class, sub)

  files <- list_files(dir, report = TRUE)

  if (!length(files)) return(NULL)

  subs <- subs_matrix(files)

  drop %<>% drop_rows(subs, .)

  files <- files[!drop]
  subs <- subs[, !drop, drop = FALSE]

  if (!length(files)) return(NULL)

  # order <- order_headings(subs, headings)
  # subs <- subs[order,,drop = FALSE]
  # files <- files[order]

  subs %<>% rename_headings(headings)
  subs %<>% apply(MARGIN = 2, str_to_title, locale = locale)

  subs %<>% set_headers(headers)


  # do heading numbers

  subs %<>% plyr::alply(2, str_c, collapse = "/") %>% unlist()
  subs %<>% str_replace("/*$", "") %>% str_replace("/[^/]+$", "")

  names(subs) <- names(files)
  subs
 # files
 # subs
}
