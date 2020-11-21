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
crossover <- function(.cols, .x, .fns, ..., .names = NULL){

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`crossover()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep()

  setup <- crossover_setup({{.cols}},
                       .x,
                       fns = .fns,
                       names = .names,
                       cnames = .cnames,
                       data = .data)

  vars <- setup$vars
  x <- setup$x

  if (length(vars) == 0L) {
    return(new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

  if (any(names %in% .cnames)) {
    dnames <- .cnames[.cnames %in% names]
    names_l <- ifelse(length(dnames) > 3, 3, length(dnames))

    rlang::abort(c("Problem with `crossover()`.",
                   i = "Output must not contain existing column names.",
                   x = paste0("`crossover()` tried to create the following existing column names: ",
                              paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
                              ifelse(length(dnames) > 3, " etc. ", ".")),
                   i = "If you want to transform existing columns try using `across()`.",
                   i = "If you want to change the output names use the `.names` argument"))

  }

  data <- dplyr::select(dplyr::cur_data(), dplyr::all_of(vars))

  n_cols <- length(data)
  n_x <- length(x)
  n_fns <- length(fns)
  seq_n_cols <- seq_len(n_cols)
  seq_n_x <- seq_len(n_x)
  seq_fns <- seq_len(n_fns)

  k <- 1L
  out <- vector("list", n_cols * n_x * n_fns)

  for (i in seq_n_cols) {
    col <- data[[i]]
    for(l in seq_n_x) {
      xl <- x[[l]]
      for (j in seq_fns) {
        fn <- fns[[j]]
        out[[k]] <- fn(col, xl, ...)
        k <- k + 1L
      }
    }
  }

  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


crossover_setup <- function(cols, x1, fns, names, cnames, data) {

  cols <- rlang::enquo(cols)
  vars <- tidyselect::eval_select(rlang::expr(!!cols), data)
  vars <- names(vars)

  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{col}_{x}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{col}_{x}_{fn}"
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
                                          col = rep(vars, each = length(x1) * length(fns)),
                                          x   = rep(names(x1) %||% x1, length(vars) * length(fns)),
                                          fn  = rep(names_fns, length(vars) * length(x1))),
                               repair = "check_unique")
  value <- list(vars = vars, x = x1, fns = fns, names = names)
  value
}
