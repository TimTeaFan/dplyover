cut_off <- function(.pattern, .select = NULL, .vars = NULL) {

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


get_suffix <- function(.pattern, .select = NULL, .vars = NULL){

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(across())
  }

  get_affix(.pattern = .pattern,
            .select = .select,
            side = "right",
            .vars = .vars)
}

get_prefix <- function(.pattern, .select = NULL, .vars = NULL){

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(across())
  }

  get_affix(.pattern = .pattern,
            .select = .select,
            side = "left",
            .vars = .vars)
}

get_affix <- function(.pattern, .select, side = c("right", "left"), .vars = NULL) {

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(across())
  }

  side <- match.arg(side)

  if (is.null(.select)) {
    .select <- .vars
  } else {
    .select <- grep(.select, .vars, perl = TRUE, value = TRUE)
  }

  .select <- stringr::str_pad(.select, max(nchar(.select)), side = side, pad = " ")
  .variant <- purrr::transpose(strsplit(.select, ""))
  .variant <- purrr::map_dbl(purrr::map(.variant, unique), length)
  .variant <- purrr::map(strsplit(.select, ""), ~ .x[.variant > 1])
  .variant <- purrr::map_chr(.variant, ~ paste0(.x, collapse = ""))
  .variant <- stringr::str_trim(.variant, side = side)

  .invariant <- gsub(.pattern, "", .variant, perl = TRUE)
  unique(.invariant)
}

get_values <- function(.var, .dat = NULL) {

  if (is.null(.dat) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .dat <- across()
  }

  var <- rlang::as_string(rlang::ensym(.var))

 unique(as.character(.dat[[var]]))

}

chr_sq <- function(from, to, by) {
  as.character(seq.int(from, to, by = by))
}

num <- function(x) {
  as.numeric(x)
}

