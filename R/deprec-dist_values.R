#' Select distinct values of a variable
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This functions was a [selection helpers][selection_helpers] and is replaced by
#' [unique_tidy()]. Apart from having a more meaningful name, `unique_tidy` supports
#' tidyselection
#'
#' `dist_values` was intended to be used inside all functions that accept a vector
#' as argument (that is `over()` and `crossover()` and all their variants) to extract
#' values of a variable. [dist_values()] returns all distinct values (or in the case of
#' factor variables: levels) of a variable `x` which are not `NA`.
#'
#' @param x An atomic vector or list. For [seq_range()] x must be numeric or date.
#' @param .sep  A character vector containing regular expression(s) which are used
#'   for splitting the values (works only if x is a character vector).
#' @param .sort A character string indicating which sorting scheme is to be applied
#'   to distinct values: ascending ("asc"), descending ("desc"), "none" or "levels". The
#'   default is ascending, only if x is a factor the default is "levels".
#'
#' @return
#' [dist_values()] returns a vector of the same type of x, with exception of
#' factors which are converted to type `"character"`.
#'
#' @section Examples:
#' TODO: Show how old examples can be expressed with `unique_tidy`
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' Selection helpers can be used inside `dplyover::over()` which in turn must be
#' used inside `dplyr::mutate` or `dplyr::summarise`. Let's first attach `dplyr`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' `dist_values()` extracts all distinct values of a column variable.
#' This is helpful when creating dummy variables in a loop using `over()`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(dist_values(Species),
#'               ~ if_else(Species == .x, 1, 0)
#'               ),
#'          .keep = "none")
#' ```
#'
#' `dist_values()` is just a wrapper around unique. However, it has five
#' differences:
#'
#' (1) `NA` values are automatically stripped. Compare:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' unique(c(1:3, NA))
#' dist_values(c(1:3, NA))
#' ```
#'
#' (2) Applied on factors, `dist_values()` returns all distinct `levels` as
#' character. Compare the following:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' fctrs <- factor(c(1:3, NA), levels = c(3:1))
#'
#' fctrs %>% unique() %>% class()
#'
#' fctrs %>% dist_values() %>% class()
#' ```
#'
#' (3) As default, the output is sorted in ascending order for non-factors, and
#' is sorted as the underyling "levels" for factors. This can be controlled by
#' setting the `.sort` argument. Compare:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # non-factors
#' unique(c(3,1,2))
#'
#' dist_values(c(3,1,2))
#' dist_values(c(3,1,2), .sort = "desc")
#' dist_values(c(3,1,2), .sort = "none")
#'
#' # factors
#' fctrs <- factor(c(2,1,3, NA), levels = c(3:1))
#'
#' dist_values(fctrs)
#' dist_values(fctrs, .sort = "levels")
#' dist_values(fctrs, .sort = "asc")
#' dist_values(fctrs, .sort = "desc")
#' dist_values(fctrs, .sort = "none")
#'
#' ```
#'
#' (4) When used on a character vector `dist_values` can take a separator
#' `.sep` to split the elements accordingly:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' c("1, 2, 3",
#'   "2, 4, 5",
#'   "4, 1, 7") %>%
#'   dist_values(., .sep = ", ")
#' ```
#'
#' (5) When used on lists `dist_values` automatically simplifiies its input
#' into a vector using `unlist`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' list(a = c(1:4), b = (4:6), c(5:10)) %>%
#'   dist_values()
#' ```
#' @keywords internal
#' @export
dist_values <- function(x, .sep = NULL, .sort = c("asc", "desc", "none", "levels")) {
  lifecycle::deprecate_warn("0.1.0", "dist_values()", "unique_tidy()")

  is_null <- identical(.sort, c("asc", "desc", "none", "levels"))
  sort <- match.arg(.sort)

  if (is.list(x)) {
    x <- unlist(x)
  }
  if (!is.null(.sep)) {
    x <- unlist(strsplit(x, .sep))
  }

  res <- as.vector(na.omit(unique(x)))
  if (!is.factor(x)) {
    if (sort == "asc") {
      return(sort(res))
    } else if (sort == "desc") {
      return(sort(res, decreasing = TRUE))
    } else {
      return(res)
    }
  } else {
    x <- levels(x)
    if (is_null || sort == "levels") {
      return(x)
    } else if (sort == "asc") {
      return(sort(x))
    } else if (sort == "desc") {
      return(sort(x, decreasing = TRUE))
    } else {
      res
    }
  }
}
