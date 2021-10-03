#' Extract Unique Elements with Tidyselect
#'
#' @description
#'
#' `unique_tidy` is a [selection helper][selection_helpers]. It is intended
#' to be used inside all functions that accept a vector or a function as argument
#' (that is `over()`, `crossover()` and `crossfun()` as well as all their variants)
#' to extract unique values of a variable.
#'
#' @param x An atomic vector, list or (if inside dplyr) a <[`tidy-select`][dplyr_tidy_select]>
#'   expression.
#' @param sort A character string indicating which sorting scheme is to be applied
#'   to extracted values: "none", ascending ("asc") or descending ("desc"). The
#'   default is "none".
#' @param sep A character vector containing regular expression(s) to use for
#'   splitting values (works only if `x` is a character vector).
#' @na.rm Logical. Should missing values (including NaN) be removed? Default is `TRUE`.
#' @grp_data Controls the behavior when applied to grouped data. Displays a warning ("warn")
#'   as default, that 'only values unique to the current group are extracted'. This warning can
#'   be silenced with "silent". `r lifecycle::badge("experimental")` Can be set to "ungroup"
#'   to access the underlying ungrouped data. This feature is highly experimental, use with
#'   care.
#'
#' @return
#' Returns a vector of the same type of x, with exception of factors which are converted
#' to type `"character"`.
#'
#' @section Note:
#' `unique_tidy()` can be used outside {dplyover} or {dplyr} but in that case it won't
#' understand <[`tidy-select`][dplyr_tidy_select]>.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' Let's first attach `dplyr`:
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
#' (1) `NA` values are automatically stripped. This behavior can be controlled with `na.rm`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' unique(c(1:3, NA))
#' unique_tidy(c(1:3, NA))
#' unique_tidy(c(1:3, NA), na.rm = FALSE)
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
#' (3) As default, the output is not sorted. The output of non-factor variables will
#' is ordered similar to `unique` in the order of appearance. The output of factors is
#' sorted as the underlying "levels". This behavior can be controlled by setting the `sort`
#' argument. Compare:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # non-factors
#' unique(c(2,1,3))
#'
#' unique_tidy(c(2,1,3))
#' unique_tidy(c(2,1,3), sort = "asc")
#' unique_tidy(c(2,1,3), sort = "desc")
#'
#' # factors
#' fctrs <- factor(c(2,1,3, NA), levels = c(3:1))
#'
#' unique_tidy(fctrs)
#' unique_tidy(fctrs, sort = "asc")
#' unique_tidy(fctrs, sort = "desc")
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
#' (6) Inside {dplyr} `unique_tidy` understands tidyselect syntax. The following call we
#' summarise all unique values in the columns `cyl`, `gear` and `carb`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' mtcars %>%
#'   summarise(values = unique_tidy(c(cyl, gear, carb), "asc"))
#' ```
#'
#' (7) Applied to group data:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' ```
#' #' @export
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
    is_grouped <- get_n_groups(dplyr_call) > 1L
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
    x <- strsplit(x, split = sep)
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
