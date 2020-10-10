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
#' @param x An atomic vector. For [seq_range()] the vector must be numeric.
#' @param .sort A character string indicating which sorting scheme is to be applied
#'   to distinct values: ascending ("asc" = default), descending ("desc") or "none".
#' @param .by A number representing the increment of the sequence.
#'
#' @return
#' An atomic vector. [seq_range()] returns an numeric vector.
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

  if (!is.numeric(x)) {
    rlang::abort(
      c("Problem with `seq_range()` input `x`.",
        i = "`x` must be a numeric vector.",
        x = paste0("`x` is of class: ", class(x), "."))
      )
  }

  .range <- range(x)

  if (identical(.by, round(.by, 0))) {
    .range[1] <- ceiling(.range[1])
    .range[2] <- floor(.range[2])
  }

  seq.int(.range[1], .range[2], by = .by)

}
