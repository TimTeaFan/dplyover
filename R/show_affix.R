#' Show affixes for variable pairs of two sets of columns
#'
#' @description
#'
#' These functions show the prefixes or suffixes for each pair of variables of
#' two sets of columns. They are intended to be used either (1) in case `across2`
#' throws an error when `{pre}` or `{suf}` are specified in `across2`'s `.names`
#' argument or (2) before using `{pre}` or `{suf}` in `across2` to understand
#' how the pre- or suffixes will look like.
#'
#' * [show_prefix()] lists each variable pair and the corresponding alphanumeric prefix
#'
#' * [show_suffix()] lists each variable pair and the corresponding alphanumeric suffix
#'
#' @param .data A data frame.
#' @param .xcols,.ycols <[`tidy-select`][dplyr::dplyr_tidy_select]> Sets of
#'   columns for which the common pre- or suffix will be shown for each pair.
#'   Note that you can not select.
#'
#' @return
#' A tibble with three columns: .xcols, .ycols and prefix or suffix.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' Below two use cases of `show_prefix/suffix` are briefly explained.
#' Let's first attach dplyr and get ready:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' ## (1) When called after an error is thrown by `across2`
#'
#' Let's assume we use `across2` with the `{pre}` glue specification on some
#' data where not all variable pairs share a common prefix. In the example below
#' we use `dplyr::rename` to create such a case. Then `across2` will throw an
#' error. The error message already suggests that we can run `show_prefix()`
#' to see what went wrong. In this case we can call `show_prefix()` without
#' any arguments:
#'
#' ```{r, comment = "#>", collapse = TRUE, error = TRUE}
#'  iris %>%
#'    as_tibble %>%
#'    rename("Pesal.Length" = Sepal.Length) %>%
#'    mutate(across2(ends_with("Length"),
#'                   ends_with("Width"),
#'                   .fns = list(product = ~ .x * .y,
#'                               sum = ~ .x + .y),
#'                   .names = "{pre}_{fn}"))
#' show_prefix()
#' ```
#'
#' ## (2) When called on a data.frame
#'
#' When called on a data.frame we just need to specify two sets of columns:
#' `.xcols` and `.ycols` (just like in `across2`).
#'
#' ```{r, comment = "#>", collapse = TRUE}
#'  iris %>%
#'    show_suffix(starts_with("Sepal"),
#'                starts_with("Petal"))
#' ```
#'
#'
#' @name show_affix
NULL
#' @rdname show_affix
#' @export
show_prefix <- function(.data = NULL, .xcols = NULL, .ycols = NULL) {

  if (is.null(.data) && !is.null(dplyover:::.last$value)) {

    .data  <- .last$value$data
    .xcols <- .last$value$xcols
    .ycols <- .last$value$ycols

    rm(value, envir = .last)

  } else {
    .xcols <- rlang::enexpr(.xcols)
    .ycols <- rlang::enexpr(.ycols)
  }

  show_affix(data = .data,
             xcols = .xcols,
             ycols = .ycols,
             type = "prefix")
}

#' @rdname show_affix
#' @export
show_suffix <- function(.data = NULL, .xcols = NULL, .ycols = NULL) {

  if (is.null(.data) && !is.null(dplyover:::.last$value)) {

    .data  <- .last$value$data
    .xcols <- .last$value$xcols
    .ycols <- .last$value$ycols

    rm(value, envir = .last)

  } else {
    .xcols <- rlang::enexpr(.xcols)
    .ycols <- rlang::enexpr(.ycols)
  }
  show_affix(data = .data,
             xcols = .xcols,
             ycols = .ycols,
             type = "suffix")
}


show_affix <- function(data, xcols, ycols, type = c("prefix", "suffix")) {

  group_vars <- group_vars(data)

  if (length(group_vars) > 0) {
    data <- dplyr::ungroup(data)
    data <- dplyr::select(data, -dplyr::all_of(group_vars))
  }

  xvars <- tidyselect::eval_select(xcols, data)
  yvars <- tidyselect::eval_select(ycols, data)

  xvars <- names(xvars)
  yvars <- names(yvars)

  if (length(xvars) != length(yvars)) {
    rlang::abort(c(paste0("Problem with `show_", type,"()` input `.xcols` and `.ycols`."),
                   i = "Input `.xcols` and `.ycols` must have the same number of columns.",
                   x = paste0(length(xvars), " columns are selected in `.xcols`, ",
                              "while ", length(yvars), " columns are selected in `.ycols`.")))
  }

  var_nms <- purrr::flatten(purrr::map2(xvars, yvars, ~ list(c(.x, .y))))
  if (type == "prefix") {
    res <- purrr::map(var_nms, ~ get_affix(.x, "prefix"))
  } else {
    res <- purrr::map(var_nms, ~ get_affix(.x, "suffix"))
  }

  res <- unlist(purrr::modify_if(res, rlang::is_empty, ~ NA_character_))

  inp_tbl <- tibble::tibble(.xcols = xvars,
                            .ycols = yvars,
                            !! type := res)

  print_min <- getOption("tibble.print_min") %||% 10
  print_max <- getOption("tibble.print_max") %||% 20

  if (nrow(inp_tbl) > print_max) {
    cat("Use `.Last.value %>% View()` to see to full list of variables.")
  }
  inp_tbl
}

# helper function for across2_setup
get_affix <- function(x, type = c("prefix", "suffix")) {

  side <- switch(type,
                 "prefix" = "right",
                 "suffix" = "left")

  x <- stringr::str_pad(x, max(nchar(x)), side = side, pad = " ")
  x_ls <- purrr::transpose(strsplit(x, ""))
  x_ls_length <- purrr::map_dbl(purrr::map(x_ls, unique), length)
  x_rle <- rle(x_ls_length)

  if (side == "right" && x_rle$values[1] == 1) {
    res <- stringr::str_sub(x[[1]],
                            start = 1L,
                            end = x_rle$length[1])

  } else if (side == "left" && x_rle$values[length(x_rle$values)] == 1) {
    res_start <- sum(x_rle$length[-length(x_rle$length)]) + 1
    res_length <- x_rle$length[length(x_rle$length)]
    res_end <- res_start + res_length

    res <- stringr::str_sub(x[[1]],
                            start = res_start,
                            end = res_end)
  } else {
    res <- NULL
  }

  res <- stringr::str_remove_all(res, "[:punct:]*$")
  res <- stringr::str_remove_all(res, "^[:punct:]*")

  if (side == "right") {
    res <- stringr::str_extract(res, "^[:alnum:]*")
  } else {
    res <- stringr::str_extract(res, "[:alnum:]*$")
  }

  res

}

# add to tests
# x <- c("Sepal.Length", "Sepal.Width")
# x <- c("Length.Sepal", "Width.Sepal")
# x <- c("Length.of.Sepal.here", "Length.no.Sepal.here")
# get_affix(x, "suffix")
