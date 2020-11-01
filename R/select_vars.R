#' Select string parts or patterns of column names
#'
#' @description
#'
#' These functions are [selection helpers][selection_helpers]. They are intended
#' to be used inside `over()` to extract parts or patterns of the column names of
#' the underlying data.
#'
#' * [cut_names()] selects strings by removing (cutting off) the specified `.pattern`.
#' This functionality resembles `stringr::str_remove_all`.
#'
#' * [extract_names()] selects strings by extracting the specified `.pattern`.
#' This functionality resembles `stringr::str_extract`.
#'
#' @param .pattern Pattern to look for.
#' @param .vars A charactor vector with variables names. When used inside `over`
#'   all column names of the underlying data are automatically supplied to `.vars`.
#'   This argument is useful when testing the functionality outside the context of
#'   `over()`.
#' @param .select Pattern to further select and narrow down the variable names
#'   provided in `.vars`. When this argument is provided the variables names in
#'   `.vars` will be narrowed down those who match the pattern specified in `.select`.
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
#'# test get_suffix, successful, but warnings need to be addressed
#'
#'


iris_tbl %>%
  mutate(over(c("Sepal", "Petal"),
              ~ .("{.x}.Width") * .("{.x}.Length"),
              .names = "Product_{vec}"))

csatraw %>%
  transmute(over(extract_names("item\\d", "[2-9]\\w$"),
                 ~ .("{.x}a") * .("{.x}b"),
                 .names = "Product_{vec}")
  )


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
extract_names <- function(.pattern, .select = NULL, .vars = NULL) {

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
         c("Problem with `extract_names()` input `.select`.",
           i = paste0("The character string provided in `.select` ('",
                      .select, "') must at least match one ",
                      ifelse(is.null(.vars), "column name.", "element in `.vars`.")),
           x = "No match was found."))
     }
   }

  .extract <- regexpr(.pattern, .selected, perl = TRUE)
  .res <- regmatches(.selected, .extract)

  if (length(.res) == 0) {
    rlang::abort(
      c("Problem with `extract_names()` input `.pattern`.",
        i = paste0("The character string provided in `.pattern` ('",
                   .pattern, "') must at least return one match."),
        x = "No match was found."))
  }

  unique(.res)

}
