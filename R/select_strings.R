#' Select string parts or patterns of column names
#'
#' @description
#'
#' These functions are [string selection helpers][string_selection_helpers].
#' They are intended to be used inside `over()` to extract parts or patterns of
#' the column names of the underlying data.
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
#' @param .remove Pattern to remove from the variable names provided in `.vars`.
#'   When this argument is provided, all variables names in `.vars` that match
#'   the pattern specified in `.remove` will be removed, before the `.pattern` to
#'   look for will be applied.
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
#'             .names = "Product_{x}"))
#'
#' iris %>%
#'   mutate(over(extract_names("Length|Width"),
#'               ~.("Petal.{.x}") * .("Sepal.{.x}"),
#'              .names = "Product_{x}"))
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
#' The survey has several 'item's consisting of two sub-questions / variables 'a'
#' and 'b'. Lets say we want to calculate the product of those two variables for
#' each item. `extract_names()` helps us to select all variables containing
#' 'item' followed by a digit using the regex `"item\\d"` as `.pattern`.
#' However, there is 'item1'  and 'item1_open' which are not followed by `a` and
#' `b`. `extract_names()` lets us exclude these items by setting the `.remove`
#' argument to `[^item1]`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' csatraw %>%
#'  transmute(over(extract_names("item\\d", "^item1"),
#'                 ~ .("{.x}a") * .("{.x}b"))
#'  )
#' ```
#' @name select_vars
NULL

#' @rdname select_vars
#' @export
cut_names <- function(.pattern, .remove = NULL, .vars = NULL) {

  .varn <- .vars

  if (is.null(.vars) && sys.call(sys.nframe() - 2)[[1]] == "meta_setup") {
    .varn <- names(dplyr::across())
  }

  if (is.null(.remove)) {
    .selected <- .varn
  } else {
    .notselected <- grep(.remove, .varn, perl = TRUE, value = TRUE)

    if (length(.notselected) == 0) {
      rlang::abort(
        c("Problem with `cut_names()` input `.remove`.",
          i = paste0("The character string provided in `.remove` ('",
                     .remove, "') must at least match one ",
                    ifelse(is.null(.vars), "column name.", "element in `.vars`.")),
          x = "No match was found."))
    }

    .selected <- setdiff(.varn, .notselected)
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
extract_names <- function(.pattern, .remove = NULL, .vars = NULL) {

   .varn <- .vars

  if (is.null(.vars) && sys.call(sys.nframe() - 2)[[1]] == "meta_setup") {
    .varn <- names(dplyr::across())
  }

   if (is.null(.remove)) {
     .selected <- .varn
   } else {
     .notselected <- grep(.remove, .varn, perl = TRUE, value = TRUE)

     if (length(.notselected) == 0) {
       rlang::abort(
         c("Problem with `extract_names()` input `.remove`.",
           i = paste0("The character string provided in `.remove` ('",
                      .remove, "') must at least match one ",
                      ifelse(is.null(.vars), "column name.", "element in `.vars`.")),
           x = "No match was found."))
     }
     .selected <- setdiff(.varn, .notselected)
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
