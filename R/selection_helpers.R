cut_names <- function(.pattern, .select = NULL, .vars = NULL) {

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(across())
  }

  if (is.null(.select)) {
    .select <- .vars
  } else {
    .select <- grep(.select, .vars, perl = TRUE, value = TRUE)
  }

  .match <- grepl(.pattern, .select, perl = TRUE)
  .extract <- gsub(.pattern, "", .select, perl = TRUE)[.match]
  unique(.extract[nchar(.extract) > 0])

}

 get_pattern <- function(.pattern, .select = NULL, .vars = NULL) {

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(across())
  }

   if (is.null(.select)) {
     .select <- .vars
   } else {
     .select <- grep(.select, .vars, perl = TRUE, value = TRUE)
   }

  .match <- grepl(.pattern, .select, perl = TRUE)
  .extract <- regexpr(.pattern, .vars, perl = TRUE)
  .res <- regmatches(.vars, .extract)

  unique(.res)

}


dist_values <- function(.var, .sort = c("asc", "desc", "none")) {

  sort <- match.arg(.sort)

  if (is.factor(.var)) {
    res <- levels(.var)
  } else {
    res <- unique(.var)
  }

  if (sort == "asc") {
    sort(res)
  } else if (sort == "desc") {
    sort(res, decreasing = TRUE)
  } else {
    res
  }

}


seq_range <- function(.var, .by) {

  .range <- range(.var)

  seq.int(ceiling(.range[1]),
          floor(.range[2]),
          by = .by)

}

