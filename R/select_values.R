#' Select values from variables
#'
#' @description
#'
#' These functions are [selection helpers][selection_helpers]. They are intended
#' to be used inside all functions that accept a vector as argument (that is `over()`
#' and `crossover()` and all their variants) to extract values of a variable.
#'
#' * [unique_tidy()] returns all distinct values (or in the case of factor variables:
#'   levels) of a variable `x` which are not `NA`.
#'
#' * [seq_range()] returns the sequence between the `range()` of a variable `x`.
#'
#' @param x An atomic vector or list. For [seq_range()] x must be numeric or date.
#' @param sep  A character vector containing regular expression(s) which are used
#'   for splitting the values (works only if x is a character vector).
#' @param sort A character string indicating which sorting scheme is to be applied
#'   to distinct values: ascending ("asc"), descending ("desc"), "none" or "levels". The
#'   default is ascending, only if x is a factor the default is "levels".
#' @param .by A number (or date expression) representing the increment of the sequence.
#'
#' @return
#' [unique_tidy()] returns a vector of the same type of x, with exception of
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
#' `unique_tidy()` extracts all distinct values of a column variable.
#' This is helpful when creating dummy variables in a loop using `over()`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(unique_tidy(Species),
#'               ~ if_else(Species == .x, 1, 0)
#'               ),
#'          .keep = "none")
#' ```
#'
#' `unique_tidy()` is just a wrapper around unique. However, it has five
#' differences:
#'
#' (1) `NA` values are automatically stripped. Compare:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' unique(c(1:3, NA))
#' unique_tidy(c(1:3, NA))
#' ```
#'
#' (2) Applied on factors, `unique_tidy()` returns all distinct `levels` as
#' character. Compare the following:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' fctrs <- factor(c(1:3, NA), levels = c(3:1))
#'
#' fctrs %>% unique() %>% class()
#'
#' fctrs %>% unique_tidy() %>% class()
#' ```
#'
#' (3) As default, the output is sorted in ascending order for non-factors, and
#' is sorted as the underyling "levels" for factors. This can be controlled by
#' setting the `sort` argument. Compare:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # non-factors
#' unique(c(3,1,2))
#'
#' unique_tidy(c(3,1,2))
#' unique_tidy(c(3,1,2), sort = "desc")
#' unique_tidy(c(3,1,2), sort = "none")
#'
#' # factors
#' fctrs <- factor(c(2,1,3, NA), levels = c(3:1))
#'
#' unique_tidy(fctrs)
#' unique_tidy(fctrs, sort = "asc")
#' unique_tidy(fctrs, sort = "desc")
#' unique_tidy(fctrs, sort = "none")
#'
#' ```
#'
#' (4) When used on a character vector `unique_tidy` can take a separator
#' `sep` to split the elements accordingly:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' c("1, 2, 3",
#'   "2, 4, 5",
#'   "4, 1, 7") %>%
#'   unique_tidy(., sep = ", ")
#' ```
#'
#' (5) When used on lists `unique_tidy` automatically simplifies its input
#' into a vector using `unlist`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' list(a = c(1:4), b = (4:6), c(5:10)) %>%
#'   unique_tidy()
#' ```
#'
#' ----------
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
unique_tidy <- function(x, sort = c("none", "asc", "desc"),
                        sep = NULL, na.rm = TRUE, grp_data = c("warn", "ungroup", "silent")) {

  # setup
  x <- rlang::enquo(x)
  grp_data <- match.arg(grp_data)
  sort <- match.arg(sort)

  # check if in dplyover
  dplyr_call <- dynGet("last_dplyr_frame", ifnotfound = NULL)

  if (!is.null(dplyr_call)) {
    in_dplyr <- TRUE
    is_grouped <- get_n_groups(dplyr_call) > 1
    # check if grouped data
      # get data
      if (is_grouped && grp_data == "ungroup") {
        dat <- get_full_data(dplyr_call)
      } else {
        if (is_grouped && grp_data == "warn") {
          rlang::warn(c("`unique_tidy()` was used on grouped data.",
                        i = "Only values unique to this group are returned.",
                        i = "Set the `grp_data` argument to `ungroup` to access the ungrouped data or to `silent` to silence this warning."))
        }
        dat <- dplyr::cur_data_all()
      }

  # if not in dplyover
  } else {

    dat <- try(dplyr::cur_data_all(), silent = TRUE)

    if (inherits(dat, "try-error")) {
      in_dplyr <- FALSE
      x <- rlang::eval_tidy(x)
    } else {
      in_dplyr <- TRUE
    }
  }

  # if in dplyover or dplyr use tidyselect
  if (in_dplyr) {
    x <- tidyselect::eval_select(x, data = dat)
    vars <- names(x)
    x <- dat[, vars]
  }

  if (is.list(x)) {
    x <- unlist(x)
  }

  if (!is.null(sep)) {
    x <- unlist(strsplit(x, sep))
  }

  # FIXME: NA's is factors not warned! move warning to the beginning and also warn factors
  # if na.rm = FALSE and NA's are present:
  if (!na.rm && any(is.na(x))) {
    if (is.factor(x)) {
      res <- c(levels(x), NA)
    } else {
      res <- unique(x)
    }

    switch(sort,
           "asc"    = return(c(sort(res), NA)),
           "desc"   = return(c(sort(res, decreasing = TRUE), NA)),
                      return(c(res))
    )

  } else {

    if (is.factor(x)) {
      res <- levels(x)
    } else {
      res <- as.vector(na.omit(unique(x)))
    }

    switch(sort,
           "asc"    = return(sort(res)),
           "desc"   = return(sort(res, decreasing = TRUE)),
                      return(res))

  }
}

#' @rdname select_values
#' @export
seq_range <- function(x, by, .by = lifecycle::deprecated()) {

  if (lifecycle::is_present(.by)) {
      lifecycle::deprecate_stop(
        when = "0.1.0",
        what = "seq_rang(.by)",
        details = "The `.by` argument has been renamed to `by` as of version 0.1.0"
      )
  }
  if (!class(x) %in% c("numeric", "integer", "Date")) {
    rlang::abort(
      c("Problem with `seq_range()` input `x`.",
        i = "`x` must be a numeric or date vector.",
        x = paste0("`x` is of class: ", class(x), "."))
      )
  }

  .range <- range(x)

  if (!is.date(x) && identical(by, round(by, 0))) {
    .range[1] <- ceiling(.range[1])
    .range[2] <- floor(.range[2])
  }

  seq(.range[1], .range[2], by = by)

}
