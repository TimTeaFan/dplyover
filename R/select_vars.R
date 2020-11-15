#' Select string parts or patterns of column names
#'
#' @description
#'
#' These functions are [selection helpers][selection_helpers]. They are intended
#' to be used inside `over()` to extract parts or patterns of the column names of
#' the underlying data.
#'
#' * [cut_names()] selects strings by removing (cutting off) the specified `.pattern`.
#' This functionality resembles `stringr::str_remove_all()`.
#'
#' * [extract_names()] selects strings by extracting the specified `.pattern`.
#' This functionality resembles `stringr::str_extract()`.
#'
#' @param .pattern Pattern to look for.
#' @param .vars A character vector with variables names. When used inside `over`
#'   all column names of the underlying data are automatically supplied to `.vars`.
#'   This argument is useful when testing the functionality outside the context of
#'   `over()`.
#' @param .select Pattern to further select and narrow down the variable names
#'   provided in `.vars`. When this argument is provided the variables names in
#'   `.vars` will be narrowed down to those which match the pattern specified in
#'   `.select`.
#'
#' @return
#' A character vector.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' Selection helpers can be used inside `dplyover::over()` which in turn must be
#' used inside `dplyr::mutate` or `dplyr::summarise`. Let's first attach `dplyr`
#' (and `stringr` for comparision):
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#' library(stringr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' Let's first compare `cut_names()` and `extract_names()`  to their {stringr}
#' equivalents `stringr::str_remove_all()` and `stringr::str_extract()`:
#'
#' We can observe two main differences:
#'
#' (1)  `cut_names()` and `extract_names()` only return strings where the function
#' was applied successfully (when characters have actually been removed or
#' extracted). `stringr::str_remove_all()` returns unmatched strings as is, while
#' `stringr::str_extract()` returns `NA`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' cut_names("Width", .vars = names(iris))
#' str_remove_all(names(iris), "Width")
#'
#' extract_names("Length|Width", .vars = names(iris))
#' str_extract(rep(names(iris), 2), "Length|Width")
#' ```
#'
#' (2) `cut_names()` and `extract_names()` return only unique values:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' cut_names("Width", .vars = rep(names(iris), 2))
#' str_remove_all(rep(names(iris), 2), "Width")
#'
#' extract_names("Length|Width", .vars = names(iris))
#' str_extract(rep(names(iris), 2), "Length|Width")
#' ```
#'
#' The examples above do not show that `cut_names()` removes *all* strings matching
#' the `.pattern` argument, while `extract_names()` does only extract the `.pattern`
#' *one* time:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' cut_names("Width", .vars = "Width.Petal.Width")
#' str_remove_all("Width.Petal.Width", "Width")
#'
#' extract_names("Width", .vars = "Width.Petal.Width")
#' str_extract("Width.Petal.Width", "Width")
#' ```
#'
#' Within [`over()`] `cut_names()` and `extract_names()` automatically use the
#' column names of the underlying data:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#' mutate(over(cut_names(".Width"),
#'             ~ .("{.x}.Width") * .("{.x}.Length"),
#'             .names = "Product_{vec}"))
#'
#' iris %>%
#'   mutate(over(extract_names("Length|Width"),
#'               ~.("Petal.{.x}") * .("Sepal.{.x}"),
#'              .names = "Product_{vec}"))
#' ```
#'
#' What problem does `cut_names()` solve?
#' In the example above using `cut_names()` might not seem helpful, since we could easily
#' use `c("Sepal", "Petal")` instead. However, there are cases where we have
#' data with a lot of similar pairs of variables sharing a common prefix or
#' suffix. If we want to loop over them using `over()` then `cut_names()` comes
#' in handy.
#'
#' The usage of `extract_names()` might be less obvious. Lets look at raw data
#' from a customer satifsaction survey which contains the following variables.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' csatraw %>% glimpse(width = 50)
#' ```
#'
#' The survey has several `item`s consisting of two sub-questions / variables `a`
#' and `b`. Lets say we want to calculate the product of those two variables for
#' each item. `extract_names()` helps us to select all variables containing
#' `"item"` followed by a digit using as regex `"item\\d"` as `.pattern`.
#' However, there is `item1` which is only one variable not followed by `a` and
#' `b`. `extract_names()` lets us exclude this item by setting the `.select`
#' argument to `[^item1]`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' csatraw %>%
#'  transmute(over(extract_names("item\\d", "[^item1]"),
#'                 ~ .("{.x}a") * .("{.x}b"))
#'  )
#' ```
#' @name select_vars
NULL

#' @rdname select_vars
#' @export
cut_names <- function(.pattern, .select = NULL, .vars = NULL) {

  .varn <- .vars

  if (is.null(.vars) && sys.call(sys.nframe() - 2)[[1]] == "over_setup") {
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

  if (is.null(.vars) && sys.call(sys.nframe() - 2)[[1]] == "over_setup") {
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
