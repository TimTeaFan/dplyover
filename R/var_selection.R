#' Extract character vectors from variable names
#'
#' @description
#' The following string selection helpers extract strings from selected variable
#' names.
#'
#' @param .ignore A character string containing a <[regular expression][base::regex]> to be
#'   replaced in the selected variable names.
#'
#' @param .select A character string containing a regular expression to subset
#'   the variable names supplied in `.vars`. If `NULL` the regular expression in
#'   `.ignore` will be applied to all variable names in `.vars`.
#'
#' @param .vars A character vector of variable names (e.g. `names(iris)`). When
#'   called within `over()` this automatically defaults to the column names of
#'   the underlying data via a call to `names(dplyr::across())`.
#'
#' @returns A character vector.
#'
#' * `cut_off()` Replaces the regular expression in `.ignore` with an empty
#'   string `""` for all selected variable names. Only unique strings that have
#'   been cut off (containing neither `NA` nor empty strings) are returned.
#'
#' * `get_suffix()` In a first step, for all selected variable names, characters
#'   starting from the left are removed if all selected variables share the same
#'   character at the same position, thus leaving only the variables suffixes.
#'   Before returning the suffixes the regular expression in `.ignore` is replaced
#'   with an empty string `""`. A look at the examples is highly recommended.
#'
#' * `get_prefix()` The same as `get_suffix()`, but here characters are removed
#'   starting from the right, thus leaving only the variables prefixes.
#'
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' The variable string selection helpers can be used within `over()` without
#' specifying the `.vars` argument. They can be used outside `over()` to test
#' whether the regular expressions are correctly specified.
#' Let's first attach `dplyr` and have a look at the names of our example data:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' names(csat)
#' ```
#'
#' We want to extract all strings before "rating" or "contract":
#' ```{r, comment = "#>", collapse = TRUE}
#' cut_off("_contact", .vars = names(csat))
#' ```
#'
#' We can use this to create new variables which rely on both "_contact" and
#' "_rating". Lets say we want to create dummy variables taking TRUE when a
#' customer had contact last year and gave a rating of "very satisfied" for
#' each of the five channels:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' csat %>%
#'   transmute(over(cut_off("contact"),
#'                  ~ case_when(.("{.x}contact") == "within last year" &
#'                                .("{.x}rating") ==  "Very Satisfied" ~ TRUE,
#'                              TRUE ~ FALSE),
#'                  .names = "{str}dummy"))
#'
#' @name var_selection
NULL

#' @export
#' @rdname var_selection
cut_off <- function(.ignore, .select = NULL, .vars = NULL) {

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(across())
  }

  if (is.null(.select)) {
    .select <- .vars
  } else {
    .select <- grep(.select, .vars, perl = TRUE, value = TRUE)
  }

  .match <- grepl(.ignore, .select, perl = TRUE)
  .extract <- gsub(.ignore, "", .select, perl = TRUE)[.match]
  unique(.extract[nchar(.extract) > 0])

}


#' @export
#' @rdname var_selection
get_suffix <- function(.select = NULL, .ignore = NULL, .vars = NULL){

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(dplyr::across())
  }

  if (is.null(.vars)) {
    rlang::abort(c("Problem with `get_suffix()` input `.vars`.",
                   i = "If not called from within `over()` `.vars` must not be `NULL`."))
  }

  get_affix(.ignore = .ignore,
            .select = .select,
            side = "right",
            .vars = .vars)
}

#' @export
#' @rdname var_selection
get_prefix <- function(.select = NULL, .ignore = NULL, .vars = NULL){

  # if (length(.select) > 0) {
  #   rlang::abort(c("Problem with `get_prefix()` input `.select`.",
  #                  i = "Regular expression in `.select` must not be longer than length == 1"))
  # }
  #
  # if (is.null(.vars)) {
  #   rlang::abort(c("Problem with `get_prefix()` input `.vars`.",
  #                  i = "If not called from within `over()` `.vars` must not be `NULL`."))
  # }

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(dplyr::across())
  }

  if (is.null(.vars)) {
    rlang::abort(c("Problem with `get_prefix()` input `.vars`.",
                   i = "If not called from within `over()` `.vars` must not be `NULL`."))
  }

  get_affix(.ignore = .ignore,
            .select = .select,
            side = "left",
            .vars = .vars)
}


get_affix <- function(.select = NULL, .ignore = NULL, side = c("right", "left"), .vars = NULL) {

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .vars <- names(dplyr::across())
  }

  side <- match.arg(side)

  if (is.null(.select)) {
    .select <- .vars
  } else {
    .select <- grep(.select, .vars, perl = TRUE, value = TRUE)
    if (is.null(.select)) {
     rlang::abort(c("Problem with `get_affix()`.",
                     x = "No variables could be selected.",
                     i = "Check if the regular expression in `.select` is actually selecting variable names."))
    }
  }

  .select <- stringr::str_pad(.select, max(nchar(.select)), side = side, pad = " ")
  .variant <- purrr::transpose(strsplit(.select, ""))
  .variant <- purrr::map_dbl(purrr::map(.variant, unique), length)
  .variant <- purrr::map(strsplit(.select, ""), ~ .x[.variant > 1])
  .variant <- purrr::map_chr(.variant, ~ paste0(.x, collapse = ""))
  .variant <- stringr::str_trim(.variant, side = side)

  .invariant <- gsub(.ignore, "", .variant, perl = TRUE)
  unique(.invariant)
}

