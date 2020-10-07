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


dist_values <- function(.var, .sort = c("asc", "desc", "none"), .dat = NULL) {

  if (is.null(.dat) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .dat <- dplyr::across()
  }

  sort <- match.arg(.sort)

  var <- rlang::as_string(rlang::ensym(.var))

  if (is.factor(.dat[[var]])) {
    res <- levels(.dat[[var]])
  } else {
    res <- unique(.dat[[var]])
  }

  if (sort == "asc") {
    sort(res)
  } else if (sort == "desc") {
    sort(res, decreasing = TRUE)
  } else {
    res
  }

}



