#' Select string parts or patterns of column names
#'
#' @description
#'
#' These functions are [selection helpers][selection_helpers].
#'
#' * [cut_names()] selects column names by cutting of the specified `.pattern`.
#'
#' * [get_pattern()] select column names by extracting the specified `.pattern`.
#'
#' @param .pattern Pattern to apply function to.
#' @param .vars A charactor vector with variables names. When used inside `over`
#'   all column names of the underlying data are automatically supplied to `.vars`.
#'   This argument is useful when testing the functionality outside the context of
#'   `over()`.
#' @param .select Pattern to further select and narrow down the variable names
#'   provided in `.vars`. When this argument is provided the variables names in
#'   `.vars` will be basically updated with a call to
#'   `grep(.select, .vars, perl = TRUE, value = TRUE)`.
#'
#' @return
#' A character vector.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' `over()` can only be used inside `dplyr::mutate` or `dplyr::summarise`.
#' It has two main use cases. They differ in how the elements in `.vec`
#' are used. Let's first attach `dplyr`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' @name select_vars
NULL

#' @rdname select_vars
#' @export
cut_names <- function(.pattern, .select = NULL, .vars = NULL) {

  .varn <- .vars

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .varn <- names(dplyr::across())
  }

  if (is.null(.select)) {
    .selected <- .varn
  } else {
    .selected <- grep(.select, .varn, perl = TRUE, value = TRUE)

    if (length(.selected) == 0) {
      rlang::abort(
        c("Problem with `cut_names()` input `.select`.",
          i = paste0("The character string provided in `.select` ('",
                     .select, "') must at least match one ",
                    ifelse(is.null(.vars), "column name.", "element in `.vars`.")),
          x = "No match was found."))
    }
  }

  .match <- grepl(.pattern, .selected, perl = TRUE)
  .extract <- gsub(.pattern, "", .selected, perl = TRUE)[.match]

  if (length(.extract) == 0) {
    rlang::abort(
      c("Problem with `cut_names()` input `.pattern`.",
         i = paste0("The character string provided in `.pattern` ('",
                    .pattern, "') must at least return one match."),
         x = "No match was found."))
  }

  unique(.extract[nchar(.extract) > 0])
}

#' @rdname select_vars
#' @export
 get_pattern <- function(.pattern, .select = NULL, .vars = NULL) {

   .varn <- .vars

  if (is.null(.vars) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .varn <- names(dplyr::across())
  }

   if (is.null(.select)) {
     .selected <- .varn
   } else {
     .selected <- grep(.select, .varn, perl = TRUE, value = TRUE)

     if (length(.selected) == 0) {
       rlang::abort(
         c("Problem with `get_pattern()` input `.select`.",
           i = paste0("The character string provided in `.select` ('",
                      .select, "') must at least match one ",
                      ifelse(is.null(.vars), "column name.", "element in `.vars`.")),
           x = "No match was found."))
     }
   }

  .match <- grepl(.pattern, .selected, perl = TRUE)
  .extract <- regexpr(.pattern, .varn, perl = TRUE)
  .res <- regmatches(.varn, .extract)

  if (length(.res) == 0) {
    rlang::abort(
      c("Problem with `get_pattern()` input `.pattern`.",
        i = paste0("The character string provided in `.pattern` ('",
                   .pattern, "') must at least return one match."),
        x = "No match was found."))
  }

  unique(.res)

}
