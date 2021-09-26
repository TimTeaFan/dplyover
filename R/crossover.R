#' Apply functions to a set of columns and a vector simultaniously in 'dplyr'
#'
#' @description
#' `crossoverx()` combines the functionality of [dplyr::across()] with [over()]
#' by iterating simultaneously over (i) a set of columns (`.xcols`) and (ii)
#' a vector or list (`.y`). `crossoverx()` *always* applies the functions in
#' `.fns` in a *nested* way to a combination of both inputs. There are, however,
#' two different ways in which the functions in `.fns` are applied.
#'
#' When `.y` is a vector or list, each function in `.fns` is applied to
#' *all pairwise combinations* between columns in `.xcols` and elements in
#' `.y` (this resembles the behavior of `over2x()` and `across2x()`).
#'
#' @param .xcols <[`tidy-select`][dplyr_tidy_select]> Columns to transform.
#'   Because `crossoverx()` is used within functions like `summarise()` and
#'   `mutate()`, you can't select or compute upon grouping variables.
#'
#' @param .y An atomic vector or list to apply functions to.
#'
#' @param .fns Functions to apply to each column in `.xcols` and element in `.y`.
#'
#'   Possible values are:
#'
#'   - A function
#'   - A purrr-style lambda
#'   - A list of functions/lambdas
#'
#'   Note that `NULL` is not accepted as argument to `.fns`.
#'
#' @param ... Additional arguments for the function calls in `.fns`.
#'
#' @param .names A glue specification that describes how to name the output
#'   columns. This can use:
#'
#'   - `{xcol}` to stand for the selected column name,
#'   - `{y}` to stand for the selected vector element, and
#'   - `{fn}` to stand for the name of the function being applied.
#'
#'   The default (`NULL`) is equivalent to `"{xcol}_{y}"` for the single function
#'   case and `"{xcol}_{y}_{fn}"` for the case where a list is used for `.fns`.
#'
#'   Note that, depending on the nature of the underlying object in `.y`,
#'   specifying `{y}` will yield different results:
#'
#'   - If `.y` is an unnamed atomic vector, `{y}` will represent each value.
#'   - If `.y` is a named list or atomic vector, `{y}` will represent each name.
#'   - If `.y` is an unnamed list, `{y}` will be the index number running from 1 to `length(y)`.
#'
#'   This standard behavior (interpretation of `{y}`) can be overwritten by
#'   directly specifying:
#'
#'   - `{y_val}` for `.y`'s values
#'   - `{y_nm}` for its names
#'   - `{y_idx}` for its index numbers
#'
#'   Alternatively, a character vector of length equal to the number of columns to
#'   be created can be supplied to `.names`. Note that in this case, the glue
#'   specification described above is not supported.
#'
#' @param .names_fn Optionally, a function that is applied after the glue
#'   specification in `.names` has been evaluated. This is, for example, helpful,
#'   in case the resulting names need to be further cleaned or trimmed.
#'
#' @returns
#' `crossoverx()` returns a tibble with one column for each combination of
#' columns in `.xcols`, elements in `.y` and functions in `.fns`.
#'
#' @seealso
#' Other members of the <[`over-across function family`][over_across_family]>.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' For the basic functionality please refer to the examples in [over()] and
#' [dplyr::across()].
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' ## Creating many similar variables for mulitple columns
#' If `.y` is a vector or list, `crossoverx()` loops every combination between
#' columns in `.xcols` and elements in `.y` over the functions in `.fns`. This
#' is helpful in cases where we want to create a batch of similar variables with
#' only slightly changes in the arguments of the calling function. A good example
#' are lagged variables. Below we create five lagged variables for each
#' 'Sepal.Length' and  'Sepal.Width'. To create nice names we use a named list
#' as argument in `.fns` and specify the glue syntax in `.names`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#'  iris %>%
#'    transmute(
#'      crossoverx(starts_with("sepal"),
#'                1:5,
#'                list(lag = ~ lag(.x, .y)),
#'                .names = "{xcol}_{fn}{y}")) %>%
#'    glimpse
#' ```
#'
#'
#'
#' @export
crossoverx <- function(.xcols = dplyr::everything(), .y, .fns, ..., .names = NULL, .names_fn = NULL){

  setup <- meta_setup(dep_call = deparse_call(sys.call()),
                      setup_fn = "crossoverx_setup",
                      cols = rlang::enquo(.xcols),
                      y1 = .y,
                      fns = .fns,
                      names = .names,
                      names_fn = .names_fn)

  vars <- setup$vars
  y <- setup$y

  if (length(vars) == 0L) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

  if (setup$dplyr_env > 0) {
    data_env <- rlang::env_parent(parent.frame(setup$dplyr_env), n = 1)
  } else {
    data_env <- rlang::as_data_mask(dplyr::cur_data())
  }

  n_cols <- length(vars)
  n_fns <- length(fns)
  seq_n_cols <- seq_len(n_cols)
  seq_fns <- seq_len(n_fns)

  k <- 1L

  n_y <- length(y)
  seq_n_y <- seq_len(n_y)
  out <- vector("list", n_cols * n_y * n_fns)

  for (i in seq_n_cols) {
    setup_env$xcol <- col <- vars[i]

    for(l in seq_n_y) {
      yl <- y[[l]]

      for (j in seq_fns) {
        fn <- fns[[j]]
        out[[k]] <- fn(get(col, envir = data_env), yl, ...)
        k <- k + 1L
      }
    }
  }

  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
  }


crossoverx_setup <- function(cols, y1, fns, names, names_fn) {

  # setup: cols
  data <- dplyr::cur_data()
  cols <- rlang::quo_set_env(cols,
                             data_mask_top(rlang::quo_get_env(cols),
                                           recursive = FALSE,
                                           inherit = TRUE))
  vars <- tidyselect::eval_select(cols, data)
  vars <- init_vars <- names(vars)

  # setup: .y

  # if .y is function:
  if (is.function(y1) || rlang::is_formula(y1)) {

      lifecycle::deprecate_stop(
        when = "0.1.0",
        what = "crossoverx(.y)",
        details = "The ability to supply a function as argument to `.y` is dropped. Please use `crossfun()` instead."
      )
    }

  y1_nm <- names(y1)
  y1_idx <- as.character(seq_along(y1))
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
    names <- names %||% "{xcol}_{y}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{xcol}_{y}_{fn}"
  }

  if (!is.list(fns)) {
    rlang::abort(c("Problem with `crossover()` input `.fns`.",
                   i = "Input `.fns` must be a function, a formula, or a list of functions/formulas."))
  }

  # use index for unnamed lists
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
  vars_no <- length(y1) * length(fns) * length(y1)
  maybe_glue <- any(grepl("{.*}", names, perl = TRUE))
  is_glue <- any(grepl("{(xcol|y|y_val|y_nm|y_idx|fn)}", names, perl = TRUE))

  # if .names use glue syntax:
  if (is_glue) {

    if (length(names) > 1) {
      rlang::abort(c("Problem with `crossover()` input `.names`.",
                     i = "Glue specification must be a character vector of length == 1.",
                     x = paste0("`.names` is of length: ", length(names), ".")))
    }

    # warn that default values are used if conditions not met
    if (is.null(y1_val) && grepl("{y_val}", names, perl = TRUE)) {
      rlang::warn("in `crossover()` `.names`: used 'y_idx' instead of 'y_val'. The latter only works with lists if all elements are length 1.")
    }

    if (is.null(y1_nm) && grepl("{y_nm}", names, perl = TRUE)) {
      rlang::warn("in `crossover()` `.names`: used 'y_idx' instead of 'y_nm', since the input object is unnamed.")
    }

      n_cols <- length(vars)
      n_y1 <- length(y1)
      n_nm_fns <- length(names_fns)
      seq_n_col <- seq_len(n_cols)
      seq_n_y1 <- seq_len(n_y1)
      seq_nm_fns <- seq_len(n_nm_fns)
      k <- 1L
      out <- vector("character", n_cols* n_y1 * n_nm_fns)

      for (i in seq_n_col) {
        for(l in seq_n_y1) {
          for (j in seq_nm_fns) {
            out[[k]] <- glue::glue(names,
                                   xcol = vars[[i]],
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

    # no correct glue syntax detected
  } else {
    # glue syntax might be wrong
    if (maybe_glue && length(names) == 1 && vars_no > 1) {
      rlang::abort(c("Problem with `crossover()` input `.names`.",
                     x = "Unrecognized glue specification `{...}` detected in `.names`.",
                     i = "`.names` only supports the following expressions: '{xcol}'. '{y}', '{y_val}', '{y_nm}', '{y_idx}' or '{fn}'."
      ))
    }
    # check if non-glue names are unique
    vctrs::vec_as_names(names, repair = "check_unique")
    # check number of names
    if (length(names) !=  vars_no) {
      rlang::abort(c("Problem with `crossover()`  input `.names`.",
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

  value <- list(vars = vars, y = y1, fns = fns, names = names)
  value
}






