#' Select values from variables
#'
#' @description
#'
#' These functions are [selection helpers][selection_helpers].
#'
#' * [dist_values()] returns all distinct values (or in the case of factor variables:
#'   levels) of a variable `x` which are not `NA`.
#'
#' * [seq_range()] returns the sequence between the `range()` of a variable `x`.
#'
#' @param x An atomic vector. For [seq_range()] x must be numeric or date.
#' @param .sort A character string indicating which sorting scheme is to be applied
#'   to distinct values: ascending ("asc" = default), descending ("desc") or "none".
#' @param .by A number (or date expression) representing the increment of the sequence.
#'
#' @return
#' [dist_values()] returns a vector of the same type of x, with exception of
#' factors which are converted to type `"character"`.
#'
#' [seq_range()] returns an vector of type `"integer"` or `"double"`.
#'
#' @section Examples:
#'
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
#' While the output in the example above is identical to `unique()`,
#' `dist_values()` has three differences:
#'
#' (1) `NA` values are automatically stripped. Compare:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' unique(c(1:3, NA))
#' dist_values(c(1:3, NA))
#' ```
#'
#' (2) Applied on factors, `dist_values()` returns all distinct `levels` as
#' character. Compare:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' factor(c(1:3, NA)) %>%
#'   as.factor() %>%
#'   unique() %>%
#'   class()
#'
#' factor(c(1:3, NA)) %>%
#'   as.factor() %>%
#'   dist_values() %>%
#'   class()
#' ```
#'
#' (3) As default, the output is sorted in ascending order. This can be
#' controlled by setting the `.sort` argument. Compare:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' unique(c(3,1,2))
#'
#' dist_values(c(3,1,2))
#' dist_values(c(3,1,2), .sort = "desc")
#' dist_values(c(3,1,2), .sort = "none")
#' ```
#'
#'
#' `seq_range()` generates a numeric sequence between the `min` and `max`
#' values of its input variable. This is helpful when creating many dummy
#' variables with varying thresholds.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(seq_range(Sepal.Length, 1),
#'               ~ if_else(Sepal.Length > .x, 1, 0),
#'               .names = "Sepal.Length.{x}"),
#'          .keep = "none")
#' ```
#'
#' Note that if the input variable does not have decimal places, `min` and `max` are
#' wrapped in `ceiling` and `floor` accordingly. This will prevent the creation of
#' variables that contain only `0` or `1`. Compare the output below with the
#' example above:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(seq(round(min(Sepal.Length), 0),
#'                   round(max(Sepal.Length), 0),
#'                   1),
#'               ~ if_else(Sepal.Length > .x, 1, 0),
#'               .names = "Sepal.Length.{x}"),
#'          .keep = "none")
#' ```
#'
#' `seq_range()` also works on dates:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' some_dates <- c(as.Date("2020-01-02"),
#'                 as.Date("2020-05-02"),
#'                 as.Date("2020-03-02"))
#'
#'
#' some_dates %>%
#'   seq_range(., "1 month")
#' ```
#'
#' @name select_values
NULL

#' @rdname select_values
#' @export
dist_values <- function(x, .sort = c("asc", "desc", "none")) {

  sort <- match.arg(.sort)

  if (is.factor(x)) {
    res <- levels(x)
  } else {
    res <- as.vector(na.omit(unique(x)))
  }

  if (sort == "asc") {
    sort(res)
  } else if (sort == "desc") {
    sort(res, decreasing = TRUE)
  } else {
    res
  }

}

#' @rdname select_values
#' @export
seq_range <- function(x, .by) {

  if (!class(x) %in% c("numeric", "integer", "Date")) {
    rlang::abort(
      c("Problem with `seq_range()` input `x`.",
        i = "`x` must be a numeric vector.",
        x = paste0("`x` is of class: ", class(x), "."))
      )
  }

  .range <- range(x)

  if (!is.date(x) && identical(.by, round(.by, 0))) {
    .range[1] <- ceiling(.range[1])
    .range[2] <- floor(.range[2])
  }

  seq(.range[1], .range[2], by = .by)

}
