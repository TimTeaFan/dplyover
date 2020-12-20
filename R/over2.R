#' Loop two inputs simultaneaously over one or several functions in 'dplyr'
#'
#' @description
#' `over2()` and `over2x()` are variants of [over()] that iterate over multiple
#' arguments simultaneously. w
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
over2 <- function(.x, .y, .fns, ..., .names = NULL, .names_fn = NULL){

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`over2()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep(type = "keep")

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
                       cnames = .cnames,
                       names_fn = .names_fn)

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

    rlang::abort(c("Problem with `over2()`.",
                   i = "Output must not contain existing column names.",
                   x = paste0("`over2()` tried to create the following existing column names: ",
                              paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
                              ifelse(length(dnames) > 3, " etc. ", ".")),
                   i = "If you want to transform existing columns try using `crossover()`, `dplyr::across()` or `across2.",
                   i = "If you want to change the output names use the `.names` argument."))

  }

  n_x <- length(x)
  n_fns <- length(fns)
  seq_n_x <- seq_len(n_x)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_x * n_fns)

  for (i in seq_n_x) {
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


over2_setup <- function(x1, y1, fns, names, cnames, names_fn) {

  # ?? still needed ??
  if (length(x1) != length(y1)) {
    rlang::abort(c("Problem with `over2()` input `.x` and `.y`.",
                   i = "Input `.x` and `.y` must have the same length.",
                   x = paste0("`.x` is of length ", length(x1),
                              ", while `.y` is of length ", length(y1), ".")))
  }

  x1_nm <- names(x1)
  y1_nm <- names(y1)

  x1_idx <- as.character(seq_along(x1))
  y1_idx <- as.character(seq_along(y1))

  x1_val <- if (is.data.frame(x1) && nrow(x1) != 1) {
    NULL
  } else if (is.list(x1) && is.vector(x1) &&
             any(purrr::map_lgl(x1, ~ length(.x) != 1))) {
    NULL
  } else {
    x1
  }

  y1_val <- if (is.data.frame(y1) && nrow(y1) != 1) {
    NULL
  } else if (is.list(y1) && is.vector(y1) &&
             any(purrr::map_lgl(y1, ~ length(.x) != 1))) {
    NULL
  } else {
    y1
  }

  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{x}_{y}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{x}_{y}_{fn}"
  }

  if (!is.list(fns)) {
    rlang::abort(c("Problem with `over()` input `.fns`.",
                   i = "Input `.fns` must be a function, a formula, or a list of functions/formulas."))
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

  if (length(names) > 1) {

    if (length(names) !=  length(x1) * length(fns)) {
      rlang::abort(c("Problem with `over2()`  input `.names`.",
                     i = "When more than one element is provided to `.names` its length must equal the number of new columns.",
                     x = paste0(length(names), " elements provided to `.names`, but the number of new columns is ", length(x1) * length(fns), ".")
      ))
    }

    if (length(names) != length(unique(names))) {

      d_names <- names[duplicated(names)]
      d_names_l <- ifelse(length(d_names) > 3, 3, length(d_names))

      rlang::abort(c("Problem with `over2()` input `.names`.",
                     i = "When more than one element is provided to `.names` all elements must be unique.",
                     x = paste0("The following names are not unique: ",
                                paste(paste0("'", d_names[seq_along(1:d_names_l)], "'"), collapse = ", "),
                                ifelse(length(d_names) > 3, " etc. ", ".")
                     )
      ))

    }

    if (!is.null(names_fn)) {
      rlang::warn("`.names_fn` will be ignored, since more than one element is provided to `.names`.")
    }

  } else {

    if (is.null(x1_val) && grepl("{x_val}", names, perl = TRUE)) {
      rlang::warn("in `over2()` `.names`: used 'x_idx' instead of 'x_val'. The latter only works with lists if all elements are length 1.")
    }

    if (is.null(x1_nm) && grepl("{x_nm}", names, perl = TRUE)) {
      rlang::warn("in `over2()` `.names`: used 'x_idx' instead of 'x_nm', since the input object is unnamed.")
    }

    if (is.null(y1_val) && grepl("{y_val}", names, perl = TRUE)) {
      rlang::warn("in `over2()` `.names`: used 'y_idx' instead of 'y_val'. The latter only works with lists if all elements are length 1.")
    }

    if (is.null(y1_nm) && grepl("{y_nm}", names, perl = TRUE)) {
      rlang::warn("in `over2()` `.names`: used 'y_idx' instead of 'y_nm', since the input object is unnamed.")
    }

  names <- vctrs::vec_as_names(glue::glue(names,
                                          x = rep(names(x1) %||% x1, each = length(fns)),
                                          x_val = rep(x1_val %||% x1_idx, each = length(fns)),
                                          x_nm = rep(x1_nm %||% x1_idx, each = length(fns)),
                                          x_idx = rep(x1_idx, each = length(fns)),

                                          y = rep(names(y1) %||% y1, each = length(fns)),
                                          y_val = rep(y1_val %||% y1_idx, each = length(fns)),
                                          y_nm = rep(y1_nm %||% y1_idx, each = length(fns)),
                                          y_idx = rep(y1_idx, each = length(fns)),

                                          fn = rep(names_fns, length(x1))),
                               repair = "check_unique")

  if (!is.null(names_fn)) {

    nm_f <- rlang::as_function(names_fn)
    names <- purrr::map_chr(names, nm_f)
  }

  }

  value <- list(x = x1, y = y1, fns = fns, names = names)
  value
}

#' @rdname over2
#' @export
over2x <- function(.x, .y, .fns, ..., .names = NULL, .names_fn = NULL){

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`over2x()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep(type = "keep")

  setup <- over2x_setup(.x,
                        .y,
                        fns = .fns,
                        names = .names,
                        cnames = .cnames,
                        names_fn = .names_fn)

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

    rlang::abort(c("Problem with `over2x()`.",
                   i = "Output must not contain existing column names.",
                   x = paste0("`over2x()` tried to create the following existing column names: ",
                              paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
                              ifelse(length(dnames) > 3, " etc. ", ".")),
                   i = "If you want to transform existing columns try using `crossover()`, `dplyr::across()` or `across2.",
                   i = "If you want to change the output names use the `.names` argument."))

  }

  n_x <- length(x)
  n_y <- length(y)
  n_fns <- length(fns)
  seq_n_x <- seq_len(n_x)
  seq_n_y <- seq_len(n_y)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_x * n_y * n_fns)

  for (i in seq_n_x) {
    xi <- x[[i]]
    for(l in seq_n_y) {
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


over2x_setup <- function(x1, y1, fns, names, cnames, names_fn) {

  x1_nm <- names(x1)
  y1_nm <- names(y1)

  x1_idx <- as.character(seq_along(x1))
  y1_idx <- as.character(seq_along(y1))

  x1_val <- if (is.data.frame(x1) && nrow(x1) != 1) {
    NULL
  } else if (is.list(x1) && is.vector(x1) &&
             any(purrr::map_lgl(x1, ~ length(.x) != 1))) {
    NULL
  } else {
    x1
  }

  y1_val <- if (is.data.frame(y1) && nrow(y1) != 1) {
    NULL
  } else if (is.list(y1) && is.vector(y1) &&
             any(purrr::map_lgl(y1, ~ length(.x) != 1))) {
    NULL
  } else {
    y1
  }

  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{x}_{y}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{x}_{y}_{fn}"
  }

  if (!is.list(fns)) {
    rlang::abort(c("Problem with `over2x()` input `.fns`.",
                   i = "Input `.fns` must be a function, a formula, or a list of functions/formulas."))
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

  if (length(names) > 1) {

    if (length(names) !=  length(x1) * length(fns)) {
      rlang::abort(c("Problem with `over2x()`  input `.names`.",
                     i = "When more than one element is provided to `.names` its length must equal the number of new columns.",
                     x = paste0(length(names), " elements provided to `.names`, but the number of new columns is ", length(x1) * length(fns), ".")
      ))
    }

    if (length(names) != length(unique(names))) {

      d_names <- names[duplicated(names)]
      d_names_l <- ifelse(length(d_names) > 3, 3, length(d_names))

      rlang::abort(c("Problem with `over2x()` input `.names`.",
                     i = "When more than one element is provided to `.names` all elements must be unique.",
                     x = paste0("The following names are not unique: ",
                                paste(paste0("'", d_names[seq_along(1:d_names_l)], "'"), collapse = ", "),
                                ifelse(length(d_names) > 3, " etc. ", ".")
                     )
      ))

    }

    if (!is.null(names_fn)) {
      rlang::warn("`.names_fn` will be ignored, since more than one element is provided to `.names`.")
    }

  } else {

    if (is.null(x1_val) && grepl("{x_val}", names, perl = TRUE)) {
      rlang::warn("in `over2x()` `.names`: used 'x_idx' instead of 'x_val'. The latter only works with lists if all elements are length 1.")
    }

    if (is.null(x1_nm) && grepl("{x_nm}", names, perl = TRUE)) {
      rlang::warn("in `over2x()` `.names`: used 'x_idx' instead of 'x_nm', since the input object is unnamed.")
    }

    if (is.null(y1_val) && grepl("{y_val}", names, perl = TRUE)) {
      rlang::warn("in `over2x()` `.names`: used 'y_idx' instead of 'y_val'. The latter only works with lists if all elements are length 1.")
    }

    if (is.null(y1_nm) && grepl("{y_nm}", names, perl = TRUE)) {
      rlang::warn("in `over2x()` `.names`: used 'y_idx' instead of 'y_nm', since the input object is unnamed.")
    }

  names <- vctrs::vec_as_names(glue::glue(names,
                                          x = rep(names(x1) %||% x1, each = length(y1) * length(fns)),
                                          x_val = rep(x1_val %||% x1_idx, each = length(y1) * length(fns)),
                                          x_nm = rep(x1_nm %||% x1_idx, each = length(y1) * length(fns)),
                                          x_idx = rep(x1_idx, each = length(y1) * length(fns)),

                                          y = rep(names(y1) %||% y1, length(x1) * length(fns)),
                                          y_val = rep(y1_val %||% y1_idx, each = length(x1) * length(fns)),
                                          y_nm = rep(y1_nm %||% y1_idx, each = length(x1) * length(fns)),
                                          y_idx = rep(y1_idx, each = length(x1) * length(fns)),

                                          fn = rep(names_fns, length(x1) * length(y1))),
                               repair = "check_unique")

    if (!is.null(names_fn)) {

    nm_f <- rlang::as_function(names_fn)
    names <- purrr::map_chr(names, nm_f)
  }

  }
  value <- list(x = x1, y = y1, fns = fns, names = names)
  value
}
