#' Extract character vectors from variable names
#'
#' @description
#' These string selection helpers extract strings according to a given pattern
#' (`.pattern`) of selected (`.select`) variable names (`.vars`):
#'
#' * `cut_off()` Truncates variable names  `.pattern` of
#'
#' * `get_suffix()` ...
#'
#' * `get_prefix()` ...
#'
#' * `get_affix()` ...
#'
#' * ...
#'
#' @param .pattern A character string containing a <[regular expression][base::regex]> to be
#'   matched in the selected variable names.
#'
#' @param .select A character string containing a regular expression to subset
#'   the variable names supplied in `.vars`. If `NULL` the `.pattern` will be
#'   applied to all variable names.
#'
#' @param .vars A character vector of variable names, e.g. names(iris). When
#'   called within `over()` this automatically defaults to the column names of
#'   the underlying data via a call to `names(dplyr::across())`.
#'
#' @param side Can be set to `right` or `left` defining from which side variable
#'   names should be considered as invariant
#'
#' @return A character vector.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' `over()` can only be used inside `dplyr::mutate` or `dplyr::summarise`.
#' It has two main use cases. They differ in how the strings in `.strs`
#' are used. Let's first attach `dplyr`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#' #' @name string_select
NULL

#' @export
#' @rdname string_select
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


#' @export
#' @rdname string_select
get_suffix <- function(.pattern, .select = NULL, .vars = NULL){

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(dplyr::across())
  }

  get_affix(.pattern = .pattern,
            .select = .select,
            side = "right",
            .vars = .vars)
}

#' @export
#' @rdname string_select
get_prefix <- function(.pattern, .select = NULL, .vars = NULL){

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(dplyr::across())
  }

  get_affix(.pattern = .pattern,
            .select = .select,
            side = "left",
            .vars = .vars)
}

#' @export
#' @rdname string_select
get_affix <- function(.pattern, .select, side = c("right", "left"), .vars = NULL) {

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(dplyr::across())
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

