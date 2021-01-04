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
across2 <- function(.xcols, .ycols, .fns, ..., .names = NULL, .names_fn = NULL){

  .data <- tryCatch({
    dplyr::cur_data()
  }, error = function(e) {
    rlang::abort("`across2()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep()

  setup <- across2_setup({{.xcols}},
                         {{.ycols}},
                         fns = .fns,
                         names = .names,
                         cnames = .cnames,
                         data = .data,
                         names_fn = .names_fn)

  xvars <- setup$xvars
  yvars <- setup$yvars

  if (length(xvars) == 0L && length(yvars) == 0L) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

  xdata <- dplyr::select(dplyr::cur_data(), dplyr::all_of(xvars))
  ydata <- dplyr::select(dplyr::cur_data(), dplyr::all_of(yvars))

  n_xcols <- length(xdata)
  n_fns <- length(fns)
  seq_n_xcols <- seq_len(n_xcols)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_xcols * n_fns)

  for (i in seq_n_xcols) {
    xcol <- xdata[[i]]
    ycol <- ydata[[i]]
    for (j in seq_fns) {
      fn <- fns[[j]]
      out[[k]] <- fn(xcol, ycol, ...)
      k <- k + 1L
    }
  }
  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


across2_setup <- function(xcols, ycols, fns, names, cnames, data, names_fn) {

  # setup: cols
  xcols <- rlang::enquo(xcols)
  ycols <- rlang::enquo(ycols)
  xcols <- rlang::quo_set_env(xcols,
                              data_mask_top(rlang::quo_get_env(xcols),
                                            recursive = FALSE,
                                            inherit = TRUE))
  ycols <- rlang::quo_set_env(ycols,
                              data_mask_top(rlang::quo_get_env(ycols),
                                            recursive = FALSE,
                                            inherit = TRUE))
  xvars <- tidyselect::eval_select(xcols, data)
  yvars <- tidyselect::eval_select(ycols, data)
  xvars <- names(xvars)
  yvars <- names(yvars)

  # check lengths
  if (length(xvars) != length(yvars)) {
    rlang::abort(c("Problem with `across2()` input `.xcols` and `.ycols`.",
                   i = "Input `.xcols` and `.ycols` must use the same number of columns.",
                   x = paste0(length(xvars), " columns are selected in `.xcols`, ",
                              ", while ", length(yvars), " columns are selected in `.ycols`.")))
  }

  # apply `.names` smart default
  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{xcol}_{ycol}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{xcol}_{ycol}_{fn}"
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
  vars_no <- length(xvars) * length(fns)
  maybe_glue <- any(grepl("{.*}", names, perl = TRUE))
  is_glue <- any(grepl("{(xcol|ycol|fn|pre|suf)}", names, perl = TRUE))

  # if .names use glue syntax:
  if (is_glue) {

    if (length(names) > 1) {
      rlang::abort(c("Problem with `across2()` input `.names`.",
                     i = "Glue specification must be a character vector of length == 1.",
                     x = paste0("`.names` is of length: ", length(names), ".")))
    }

    # setup pre and suf
    names2 <- names %||% ""
    pre1 <- NULL
    suf1 <- NULL

    # check pre and suf
    check_pre <- grepl("{pre}", names2, perl = TRUE) # any / all ?
    check_suf <- grepl("{suf}", names2, perl = TRUE)

    if (check_pre || check_suf) {

      if (is.function(fns) || rlang::is_formula(fns)) {
        names2 <- "{xcol}_{ycol}"
        fns <- list(`1` = fns)
      } else {
        names2 <- "{xcol}_{ycol}_{fn}"
      }

      var_nms <- purrr::flatten(purrr::map2(xvars, yvars, ~ list(c(.x, .y))))
      pre1 <- purrr::map_chr(var_nms, ~ get_affix(.x, "prefix"))
      suf1 <- purrr::map_chr(var_nms, ~ get_affix(.x, "suffix"))

      if (check_pre && length(pre1) < 1) {
        rlang::abort(c("Problem with `across2()` input `.names`.",
                       i = "When `{pre}` is used inside `.names` the input variables in `.xcols` and `.ycols` must share a common prefix of length > 0.",
                       x = "No shared prefix could be extracted.",
                       i = "Use `test_prefix()` to check why the extraction doesn't yield the expected result."))
      }
      if (check_suf && length(suf1) < 1) {
        rlang::abort(c("Problem with `across2()` input `.names`.",
                       i = "When `{suf}` is used inside `.names` the input variables in `.xcols` and `.ycols` must share a common suffix of length > 0.",
                       x = "No shared suffix could be extracted.",
                       i = "Use `test_suffix()` to check why the extraction doesn't yield the expected result."))
      }
    }

    names <- vctrs::vec_as_names(glue::glue(names,
                                            xcol = rep(xvars, each = length(fns)),
                                            ycol = rep(yvars, each = length(fns)),
                                            pre = rep(pre1, each = length(fns)),
                                            suf = rep(suf1, each = length(fns)),
                                            fn = rep(names_fns, length(xcols))), # here vars1 instead
                                 repair = "check_unique")

    # no correct glue syntax detected
  } else {
    # glue syntax might be wrong
    if (maybe_glue && length(names) == 1 && vars_no > 1) {
      rlang::abort(c("Problem with `across2()`  input `.names`.",
                     x = "Unrecognized glue specification `{...}` detected in `.names`.",
                     i = "`.names` only supports the following expressions: '{xcol}'. '{ycol}' or '{fn}'."
      ))
    }
    # check if non-glue names are unique
    vctrs::vec_as_names(names, repair = "check_unique")
    # check number of names
    if (length(names) !=  vars_no) {
      rlang::abort(c("Problem with `across2()`  input `.names`.",
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

  value <- list(xvars = xvars, yvars = yvars, fns = fns, names = names)
  value
}


across2x <- function(.xcols, .ycols, .fns, ..., .names = NULL, .names_fn = NULL){

  .data <- tryCatch({
    dplyr::cur_data()
  }, error = function(e) {
    rlang::abort("`across2x()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep()

  setup <- across2x_setup({{.xcols}},
                         {{.ycols}},
                         fns = .fns,
                         names = .names,
                         cnames = .cnames,
                         data = .data,
                         names_fn = .names_fn)

  xvars <- setup$xvars
  yvars <- setup$yvars

  if (length(xvars) == 0L && length(yvars)) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

  xdata <- dplyr::select(dplyr::cur_data(), dplyr::all_of(xvars))
  ydata <- dplyr::select(dplyr::cur_data(), dplyr::all_of(yvars))

  n_xcols <- length(xdata)
  n_ycols <- length(ydata)
  n_fns <- length(fns)
  seq_n_xcols <- seq_len(n_xcols)
  seq_n_ycols <- seq_len(n_ycols)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_xcols * n_ycols * n_fns)

  for (i in seq_n_xcols) {
    xcol <- xdata[[i]]
    for(l in seq_n_ycols) {
      ycol <- ydata[[l]]
      for (j in seq_fns) {
        fn <- fns[[j]]
        out[[k]] <- fn(xcol, ycol, ...)
        k <- k + 1L
      }
    }
  }

  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


across2x_setup <- function(xcols, ycols, fns, names, cnames, data, names_fn) {

  # setup: cols
  xcols <- rlang::enquo(xcols)
  ycols <- rlang::enquo(ycols)
  xcols <- rlang::quo_set_env(xcols,
                              data_mask_top(rlang::quo_get_env(xcols),
                                            recursive = FALSE,
                                            inherit = TRUE))
  ycols <- rlang::quo_set_env(ycols,
                              data_mask_top(rlang::quo_get_env(ycols),
                                            recursive = FALSE,
                                            inherit = TRUE))
  xvars <- tidyselect::eval_select(xcols, data)
  yvars <- tidyselect::eval_select(ycols, data)
  xvars <- names(xvars)
  yvars <- names(yvars)

  # apply `.names` smart default
  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{xcol}_{ycol}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{xcol}_{ycol}_{fn}"
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
  vars_no <- length(xvars) * length(yvars) * length(fns)
  maybe_glue <- any(grepl("{.*}", names, perl = TRUE))
  is_glue <- any(grepl("{(xcol|ycol|fn)}", names, perl = TRUE))

  # if .names use glue syntax:
  if (is_glue) {

    if (length(names) > 1) {
      rlang::abort(c("Problem with `crossover()` input `.names`.",
                     i = "Glue specification must be a character vector of length == 1.",
                     x = paste0("`.names` is of length: ", length(names), ".")))
    }

    n_xcols <- length(xvars)
    n_ycols <- length(yvars)
    n_nm_fns <- length(names_fns)
    seq_n_xcols <- seq_len(n_xcols)
    seq_n_ycols <- seq_len(n_ycols)
    seq_nm_fns <- seq_len(n_nm_fns)
    k <- 1L
    out <- vector("character", n_xcols * n_ycols * n_nm_fns)

    for (i in seq_n_xcols) {
      for(l in seq_n_ycols) {
        for (j in seq_nm_fns) {
          out[[k]] <- glue::glue(names,
                                 xcol = xvars[[i]],
                                 ycol = yvars[[l]],
                                 fn = names_fns[[j]])
          k <- k + 1L
        }
      }
    }

    names <- vctrs::vec_as_names(out, repair = "check_unique") # unique?

  # no correct glue syntax detected
  } else {
    # glue syntax might be wrong
    if (maybe_glue && length(names) == 1 && vars_no > 1) {
      rlang::abort(c("Problem with `across2x()` input `.names`.",
                     x = "Unrecognized glue specification `{...}` detected in `.names`.",
                     i = "`.names` only supports the following expressions: '{xcol}'. '{ycol}' or '{fn}'."
      ))
    }
    # check if non-glue names are unique
    vctrs::vec_as_names(names, repair = "check_unique")
    # check number of names
    if (length(names) !=  vars_no) {
      rlang::abort(c("Problem with `across2x()` input `.names`.",
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

  value <- list(xvars = xvars, yvars = yvars, fns = fns, names = names)
  value
}



across2x_int <- function(.xcols, .ycols, .fns, .data, ..., .names = NULL){

  .cnames <- names(.data)

  check_keep()

  setup <- across2x_setup({{.xcols}},
                          {{.ycols}},
                          fns = .fns,
                          names = .names,
                          cnames = .cnames,
                          data = .data)

  vars1 <- setup$vars1
  vars2 <- setup$vars2

  if (length(vars1) == 0L && length(vars2)) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

  data1 <- dplyr::select(dplyr::cur_data(), dplyr::all_of(vars1))
  data2 <- dplyr::select(dplyr::cur_data(), dplyr::all_of(vars2))

  n_xcols <- length(data1)
  n_ycols <- length(data2)
  n_fns <- length(fns)
  seq_n_xcols <- seq_len(n_xcols)
  seq_n_ycols <- seq_len(n_ycols)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_xcols * n_ycols * n_fns)

  for (i in seq_n_xcols) {
    xcol <- data1[[i]]
    for(l in seq_n_ycols) {
      ycol <- data2[[l]]
      for (j in seq_fns) {
        fn <- fns[[j]]
        out[[k]] <- fn(xcol, ycol, ...)
        k <- k + 1L
      }
    }
  }

  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}
