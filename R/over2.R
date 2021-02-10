#' Apply functions to two vectors simultaniously in 'dplyr'
#'
#' @description
#' `over2()` and `over2x()` are variants of [over()] that iterate over two
#' objects simultaneously. `over2()` loops each *pair of elements* in `.x` and
#' `.y` over one or more functions, while `over2x()` loops
#' *all pairwise combinations between elements* in `.x` a `.y` over one or more
#' functions.
#'
#' @inheritParams over
#'
#' @param .x,.y An atomic vector or list to apply functions to. Alternatively a
#'   <[`selection helper`][selection_helpers]> can be used to create a vector.
#'   `over2()` requires `.x` and `.y` to be of the same length.
#'
#' @param .fns Functions to apply to each of the elements in `.x` and `.y`. .
#'
#'   Possible values are:
#'
#'   - A function
#'   - A purrr-style lambda
#'   - A list of functions/lambdas
#'
#'   For examples see the example section below.
#'
#'   Note that `NULL` is not accepted as argument to `.fns`.
#'
#' @param .names A glue specification that describes how to name the output
#'   columns. This can use `{x}` and `{y}` to stand for the selected vector element,
#'   and `{fn}` to stand for the name of the function being applied. The default
#'   (`NULL`) is equivalent to `"{x}_{y}"` for the single function case and
#'   `"{x}_{y}_{fn}"` for the case where a list is used for `.fns`.
#'
#'   Note that, depending on the nature of the underlying object in `.x` and `.y`,
#'   specifying `{x}/{y}` will yield different results:
#'
#'   - If `.x/.y` is an unnamed atomic vector, `{x}/{y}` will represent each value.
#'   - If `.x/.y` is a named list or atomic vector, `{x}/{y}` will represent each name.
#'   - If `.x/.y` is an unnamed list, `{x}/{y}` will be the index number running
#'   from 1 to `length(x)` or `length(y)` respectively.
#'
#'   This standard behavior (interpretation of `{x}/{y}`) can be overwritten by
#'   directly specifying:
#'
#'   - `{x_val}` or `{y_val}` for `.x`'s or  `.y`'s values
#'   - `{x_nm}` or `{y_nm}` for their names
#'   - `{x_idx}` or `{y_idx}` for their index numbers
#'
#'   Alternatively, a character vector of length equal to the number of columns to
#'   be created can be supplied to `.names`. Note that in this case, the glue
#'   specification described above is not supported.
#'
#' @returns
#' `over2()` returns a tibble with one column for each pair of elements in `.x`
#' and `.y` combined with each function in `.fns`.
#'
#' `over2x()` returns a tibble with one column for each combination between elements
#' in `.x` and `.y` combined with each function in `.fns`.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' For the basic functionality please refer to the examples in [over()].
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' When doing exploratory analysis, it is often helpful to transform continious variables
#' into several categorial variables. Below we use `over2()` to loop over two lists
#' containing "breaks" and "labels" arguments, which we then use in a call to `cut()`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' brks <- list(b1 = 3:8,
#'              b2 = seq(3, 9, by = 2))
#'
#' labs <- list(l1 = c("3 to 4", "4 to 5", "5 to 6",
#'                    "6 to 7", "7 to 8"),
#'             l2 = c("3 to 5", "5 to 7", "7 to 9"))
#'
#' iris %>%
#'   transmute(over2(brks, labs,
#'                   ~ cut(Sepal.Length,
#'                         breaks = .x,
#'                         labels = .y),
#'                   .names = "Sepal.Length.cut{x_idx}"))
#' ```
#'
#' `over2x()` makes it possible to create dummy variables for interaction effects
#' of two variables. In the example below, each customer 'type' is combined with
#' each 'product' type:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' csat %>%
#'   transmute(over2x(unique(type),
#'                    unique(product),
#'                    ~ type == .x & product == .y)) %>%
#'   glimpse
#' ```
#'
#' @export
over2 <- function(.x, .y, .fns, ..., .names = NULL, .names_fn = NULL){

  grp_id <- tryCatch({
    dplyr::cur_group_id()
  }, error = function(e) {
    rlang::abort("`over2()` must only be used inside dplyr verbs.")
  })

  deparse_call <- deparse(sys.call(),
                          width.cutoff = 500L,
                          backtick = TRUE,
                          nlines = 1L,
                          control = NULL)

  setup <- meta_setup(grp_id = grp_id,
                      dep_call = deparse_call,
                      par_frame = parent.frame(),
                      setup_fn = "over2_setup",
                      x1 = .x,
                      y1 = .y,
                      fns = .fns,
                      names = .names,
                      names_fn = .names_fn)

  x <- setup$x
  y <- setup$y

  # check empty input
  if (length(x) == 0L && length(y)) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

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


over2_setup <- function(x1, y1, fns, names, names_fn) {

  if (length(x1) != length(y1)) {
    rlang::abort(c("Problem with `over2()` input `.x` and `.y`.",
                   i = "Input `.x` and `.y` must have the same length.",
                   x = paste0("`.x` is of length ", length(x1),
                              ", while `.y` is of length ", length(y1), ".")))
  }

  # setup name variants
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

  # apply `.names` smart default
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

  # use index for unnamed lists
  if (is.list(x1) && !rlang::is_named(x1)) {
    names(x1) <- x1_idx
  }
  if (is.list(y1) && !rlang::is_named(y1)) {
    names(y1) <- y1_idx
  }

  # handle formulas
  fns <- purrr::map(fns, rlang::as_function)

  # make sure fns has names, use number to replace unnamed
  if (is.null(names(fns))) {
    names_fns <- seq_along(fns)
  } else {
    names_fns <- names(fns)
    empties <- which(names_fns == "")
    if (length(empties)) {
      names_fns[empties] <- empties
    }
  }

  # setup control flow:
  vars_no <- length(x1) * length(fns)
  maybe_glue <- any(grepl("{.*}", names, perl = TRUE))
  is_glue <- any(grepl("{(x|x_val|x_nm|x_idx|y|y_val|y_nm|y_idx|fn)}", names, perl = TRUE))

  # if .names use glue syntax:
  if (is_glue) {

    # warn that default values are used if conditions not met
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

  # no correct glue syntax detected
  } else {

    if (maybe_glue && length(names) == 1 && vars_no > 1) {
      rlang::abort(c("Problem with `over2()`  input `.names`.",
                     x = "Unrecognized glue specification `{...}` detected in `.names`.",
                     i = "`.names` only supports the following expressions: '{x}', '{x_val}', '{x_nm}', '{x_idx}',  '{fn}', '{y}', '{y_val}', '{y_nm}', '{y_idx}' or '{fn}'."
      ))
    }
    # check if names are unique
    vctrs::vec_as_names(names, repair = "check_unique")
    # check number of names
    if (length(names) !=  vars_no) {
      rlang::abort(c("Problem with `over2()`  input `.names`.",
                     i = "The number of elements in `.names` must equal the number of new columns.",
                     x = paste0(length(names), " elements provided to `.names`, but the number of new columns is ", vars_no, ".")
      ))
    }
  }

  # apply names_fn
  if (!is.null(names_fn)) {

    nm_f <- rlang::as_function(names_fn)
    names <- purrr::map_chr(names, nm_f)
  }




  value <- list(x = x1, y = y1, fns = fns, names = names)
  value
}

#' @rdname over2
#' @export
over2x <- function(.x, .y, .fns, ..., .names = NULL, .names_fn = NULL){

  grp_id <- tryCatch({
    dplyr::cur_group_id()
  }, error = function(e) {
    rlang::abort("`over2x()` must only be used inside dplyr verbs.")
  })

  deparse_call <- deparse(sys.call(),
                          width.cutoff = 500L,
                          backtick = TRUE,
                          nlines = 1L,
                          control = NULL)

  setup <- meta_setup(grp_id = grp_id,
                      dep_call = deparse_call,
                      par_frame = parent.frame(),
                      setup_fn = "over2x_setup",
                      x1 = .x,
                      y1 = .y,
                      fns = .fns,
                      names = .names,
                      names_fn = .names_fn)

  x <- setup$x
  y <- setup$y

  if (length(x) == 0L && length(y)) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

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


over2x_setup <- function(x1, y1, fns, names, names_fn) {

  # setup name variants
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

  # apply `.names` smart default
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

  # use index for unnamed lists
  if (is.list(x1) && !rlang::is_named(x1)) {
    names(x1) <- x1_idx
  }
  if (is.list(y1) && !rlang::is_named(y1)) {
    names(y1) <- y1_idx
  }

  # handle formulas
  fns <- purrr::map(fns, rlang::as_function)

  # make sure fns has names, use number to replace unnamed
  if (is.null(names(fns))) {
    names_fns <- seq_along(fns)
  } else {
    names_fns <- names(fns)
    empties <- which(names_fns == "")
    if (length(empties)) {
      names_fns[empties] <- empties
    }
  }

  # setup control flow:
  vars_no <- length(x1) * length(y1) * length(fns)
  maybe_glue <- any(grepl("{.*}", names, perl = TRUE))
  is_glue <- any(grepl("{(x|x_val|x_nm|x_idx|fn)}", names, perl = TRUE))

  # if .names use glue syntax:
  if (is_glue) {

    if (length(names) > 1) {
      rlang::abort(c("Problem with `over2x()` input `.names`.",
                     i = "Glue specification must be a character vector of length == 1.",
                     x = paste0("`.names` is of length: ", length(names), ".")))
    }

    # warn that default values are used if conditions not met
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

    n_x1 <- length(x1)
    n_y1 <- length(y1)
    n_nm_fns <- length(names_fns)
    seq_n_x1 <- seq_len(n_x1)
    seq_n_y1 <- seq_len(n_y1)
    seq_nm_fns <- seq_len(n_nm_fns)
    k <- 1L
    out <- vector("character", n_x1 * n_y1 * n_nm_fns)

    for (i in seq_n_x1) {
      for(l in seq_n_y1) {
        for (j in seq_nm_fns) {
          out[[k]] <- glue::glue(names,
                                 x = names(x1)[[i]] %||% x1[[i]],
                                 x_val = x1_val[[i]] %||% x1_idx[[i]],
                                 x_nm = x1_nm[[i]] %||% x1_idx[[i]],
                                 x_idx = x1_idx[[i]],
                                 y = names(y1)[[l]] %||% y1[[l]],
                                 y_val = y1_val[[l]] %||% y1_idx[[l]],
                                 y_nm = y1_nm[[l]] %||% y1_idx[[l]],
                                 y_idx = y1_idx[[l]],
                                 fn = names_fns[[j]])
          k <- k + 1L
        }
      }
    }

    names <- vctrs::vec_as_names(out, repair = "check_unique")

  } else {
    # glue syntax might be wrong
    if (maybe_glue && length(names) == 1 && vars_no > 1) {
      rlang::abort(c("Problem with `over2x()`  input `.names`.",
                     x = "Unrecognized glue specification `{...}` detected in `.names`.",
                     i = "`.names` only supports the following expressions: '{x}', '{x_val}', '{x_nm}', '{x_idx}',  '{fn}', '{y}', '{y_val}', '{y_nm}', '{y_idx}' or '{fn}'."
      ))
    }
    # check if names are unique
    vctrs::vec_as_names(names, repair = "check_unique")
    # check number of names
    if (length(names) !=  vars_no) {
      rlang::abort(c("Problem with `over2x()`  input `.names`.",
                     i = "The number of elements in `.names` must equal the number of new columns.",
                     x = paste0(length(names), " elements provided to `.names`, but the number of new columns is ", vars_no, ".")
      ))
    }
  }

  # apply names_fn
  if (!is.null(names_fn)) {
    nm_f <- rlang::as_function(names_fn)
    names <- purrr::map_chr(names, nm_f)
  }
  value <- list(x = x1, y = y1, fns = fns, names = names)
  value
}
