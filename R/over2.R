#' Loop two inputs simultaneaously over one or several functions in 'dplyr'
#'
#' @description
#' ...
#'
#' @inheritParams over
#'
#' @param .x,.y An atomic vector (expect 'raw' and 'complex') to apply functions to.
#'   Instead of a vector a <[`selection helper`][selection_helpers]> or anything else
#'   that is coercible to an atomic vector can be used. Note that `over()` must only
#'   be used to create 'new' columns and will throw an error if `.x` contains
#'   existing column names. To transform existing columns use [dplyr::across()].
#'
#' @param ... Additional arguments for the function calls in `.fns`.
#'
#' @param .names A glue specification that describes how to name the output
#'   columns. This can use `{vec}` to stand for the selected vector element, and
#'   `{fn}` to stand for the name of the function being applied. The default
#'   (`NULL`) is equivalent to `"{vec}"` for the single function case and
#'   `"{vec}_{fn}"` for the case where a list is used for `.fns`.
#'
#' @returns
#' A tibble with one column for each element in `.x` and each function in `.fns`;.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' `over()` can only be used inside `dplyr::mutate` or `dplyr::summarise`.
#' It has two main use cases. They differ in how the elements in `.x`
#' are used. Let's first attach `dplyr`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' @export
over2 <- function(.x, .y, .fns, ..., .names = NULL){

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`over2()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep()

  # check later
  if (length(.x) != length(.y)) {
    inp <- vctrs::vec_recycle_common(.x, .y)
    .x <- inp[[1]]
    .y <- inp[[2]]
  }

  setup <- over2_setup(.x,
                       .y,
                       fns = .fns,
                       names = .names,
                       cnames = .cnames)

  x <- setup$x
  y <- setup$y

  if (length(x) == 0L && length(y)) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

  if (any(names %in% .cnames)) {
    dnames <- .cnames[.cnames %in% names]
    names_l <- ifelse(length(dnames) > 3, 3, length(dnames))

    rlang::abort(c("Problem with `over()`.",
                   i = "Output must not contain existing column names.",
                   x = paste0("`over()` tried to create the following existing column names: ",
                              paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
                              ifelse(length(dnames) > 3, " etc. ", ".")),
                   i = "If you want to transform existing columns try using `across()`.",
                   i = "If you want to change to output names use the `.names` argument"))

  }

  n_vec <- length(x)
  n_fns <- length(fns)
  seq_n_vec <- seq_len(n_vec)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_vec * n_fns)

  for (i in seq_n_vec) {
    xi <- x[[i]]
    yi <- y[[i]]
    for (j in seq_fns) {
      fn <- fns[[j]]
      out[[k]] <- fn(xi, yi, ...)
      k <- k + 1L
    }
  }
  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


over2_setup <- function(x1, y1, fns, names, cnames) {

  if(is.list(x1) && !rlang::is_named(x1)) {
    rlang::abort(c("Problem with `over()` input `.x`.",
                   i = "If `.x` is a list, it must be named.",
                   x = "`.x` is an unnamed list."))
  }

  if(is.list(y1) && !rlang::is_named(y1)) {
    rlang::abort(c("Problem with `over()` input `.y`.",
                   i = "If `.y` is a list, it must be named.",
                   x = "`.y` is an unnamed list."))
  }

  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{x}_{y}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{x}_{y}_{fn}"
  }

  if (!is.list(fns)) {
    rlang::abort(c("Problem with `over()` input `.fns`.",
                   i = "Input `.fns` must be a function or a list of functions"))
  }

  fns <- purrr::map(fns, rlang::as_function)

  if (is.null(names(fns))) {
    names_fns <- seq_along(fns)
  } else {
    names_fns <- names(fns)
    empties <- which(names_fns == "")
    if (length(empties)) {
      names_fns[empties] <- empties
    }
  }

  names <- vctrs::vec_as_names(glue::glue(names,
                                          x = rep(names(x1) %||% x1, each = length(fns)),
                                          y = rep(names(y1) %||% y1, each = length(fns)),
                                          fn = rep(names_fns, length(x1))),
                               repair = "check_unique")
  value <- list(x = x1, y = y1, fns = fns, names = names)
  value
}

#' Loop two nested inputs over one or several functions in 'dplyr'
#'
#' @description
#' `over()` makes it easy to create new colums inside a [dplyr::mutate()] or
#' [dplyr::summarise()] call by applying a function (or a set of functions) to
#' a vector using a syntax similar to [dplyr::across()]. The main difference is
#' that [dplyr::across()] transforms or creates new columns based on existing ones,
#' while `over()` creates new columns based on a vector to which it will apply one
#' or several functions. Whereas [dplyr::across()] allows `tidy-selection` helpers
#' to select columns, `over()` provides its own helper functions to select strings
#' or values based on either (1) column names or (2) values of specified columns.
#' See the examples below and the `vignette("over")` for more details.
#'
#' inherit .x from over
#' inherit .fns from over
#'
#' @param .y An atomic vector (expect 'raw' and 'complex') to apply functions to.
#'   Instead of a vector a <[`selection helper`][selection_helpers]> or anything else
#'   that is coercible to an atomic vector can be used. Note that `over()` must only
#'   be used to create 'new' columns and will throw an error if `.x` contains
#'   existing column names. To transform existing columns use [dplyr::across()].
#'
#' @param ... Additional arguments for the function calls in `.fns`.
#'
#' @param .names A glue specification that describes how to name the output
#'   columns. This can use `{vec}` to stand for the selected vector element, and
#'   `{fn}` to stand for the name of the function being applied. The default
#'   (`NULL`) is equivalent to `"{vec}"` for the single function case and
#'   `"{vec}_{fn}"` for the case where a list is used for `.fns`.
#'
#' @returns
#' A tibble with one column for each element in `.x` and each function in `.fns`;.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' `over()` can only be used inside `dplyr::mutate` or `dplyr::summarise`.
#' It has two main use cases. They differ in how the elements in `.x`
#' are used. Let's first attach `dplyr`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' @export
over2x <- function(.x, .y, .fns, ..., .names = NULL){

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`over2()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep()

  setup <- over2x_setup(.x,
                       .y,
                       fns = .fns,
                       names = .names,
                       cnames = .cnames)

  x <- setup$x
  y <- setup$y

  if (length(x) == 0L && length(y)) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

  if (any(names %in% .cnames)) {
    dnames <- .cnames[.cnames %in% names]
    names_l <- ifelse(length(dnames) > 3, 3, length(dnames))

    rlang::abort(c("Problem with `over()`.",
                   i = "Output must not contain existing column names.",
                   x = paste0("`over()` tried to create the following existing column names: ",
                              paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
                              ifelse(length(dnames) > 3, " etc. ", ".")),
                   i = "If you want to transform existing columns try using `across()`.",
                   i = "If you want to change to output names use the `.names` argument"))

  }

  n_vec_x <- length(x)
  n_vec_y <- length(y)
  n_fns <- length(fns)
  seq_n_vec_x <- seq_len(n_vec_x)
  seq_n_vec_y <- seq_len(n_vec_y)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_vec_x * n_vec_y * n_fns)

  for (i in seq_n_vec_x) {
    xi <- x[[i]]
    for(l in seq_n_vec_y) {
    yl <- y[[l]]
      for (j in seq_fns) {
        fn <- fns[[j]]
        out[[k]] <- fn(xi, yl, ...)
        k <- k + 1L
      }
    }
  }
  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


over2x_setup <- function(x1, y1, fns, names, cnames) {

  if(is.list(x1) && !rlang::is_named(x1)) {
    rlang::abort(c("Problem with `over()` input `.x`.",
                   i = "If `.x` is a list, it must be named.",
                   x = "`.x` is an unnamed list."))
  }

  if(is.list(y1) && !rlang::is_named(y1)) {
    rlang::abort(c("Problem with `over()` input `.y`.",
                   i = "If `.y` is a list, it must be named.",
                   x = "`.y` is an unnamed list."))
  }

  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{x}_{y}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{x}_{y}_{fn}"
  }

  if (!is.list(fns)) {
    rlang::abort(c("Problem with `over()` input `.fns`.",
                   i = "Input `.fns` must be a function or a list of functions"))
  }

  fns <- purrr::map(fns, rlang::as_function)

  if (is.null(names(fns))) {
    names_fns <- seq_along(fns)
  } else {
    names_fns <- names(fns)
    empties <- which(names_fns == "")
    if (length(empties)) {
      names_fns[empties] <- empties
    }
  }

  names <- vctrs::vec_as_names(glue::glue(names,
                                          x = rep(names(x1) %||% x1, each = length(y1) * length(fns)),
                                          y = rep(names(y1) %||% y1, length(x1) * length(fns)),
                                          fn = rep(names_fns, length(x1) * length(y1))),
                               repair = "check_unique")
  value <- list(x = x1, y = y1, fns = fns, names = names)
  value
}
