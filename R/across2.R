#' Apply functions to two sets of columns simultaniously in 'dplyr'
#'
#' @description
#' `across2()` and `across2x()` are variants of [`dplyr::across()`] that iterate
#' over two columns simultaneously. `across2()` loops each *pair of columns* in `.xcols`
#' and  `.ycols` over one or more functions, while `across2x()` loops
#' *every combination between columns* in `.xcols` and `.ycols` over one or more functions.
#'
#' @param .xcols,.ycols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to transform.
#'   Note that you can not select or compute upon grouping variables.
#'
#' @param .fns Functions to apply to each column in `.xcols` and `.ycols`.
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
#'   - `{xcol}` to stand for the selected column name in `.xcols`,
#'   - `{ycol}` to stand for the selected column name in `.ycols`, and
#'   - `{fn}` to stand for the name of the function being applied.
#'
#'   The default (`NULL`) is equivalent to `"{xcol}_{ycol}"` for the single function
#'   case and `"{xcol}_{ycol}_{fn}"` for the case where a list is used for `.fns`.
#'
#'   `across2()` supports two additional glue specifications: `{pre}` and `{suf}`.
#'   They extract the common alphanumeric prefix or suffix of each pair of
#'   variables.
#'
#'   Alternatively to a glue specification, a character vector of length equal
#'   to the number of columns to be created can be supplied to `.names`.
#'   Note that in this case, the glue specification described above is not supported.
#'
#' @param .names_fn Optionally, a function that is applied after the glue
#'   specification in `.names` has been evaluated. This is, for example, helpful,
#'   in case the resulting names need to be further cleaned or trimmed.
#'
#' @param .comb  In `across2x()` this argument allows to control which
#'   combinations of columns are to be created. This argument only matters, if
#'   the columns specified in `.xcols` and `.ycols` overlap to some extent.
#'
#'   - `"all"`, the default, will create all pairwise combinations between columns
#'   in `.xcols` and `.ycols` *including all permutations* (e.g.
#'   `foo(column_x, column_y)` as well as `foo(column_y, column_x)`.
#'   - `"unique"` will only create all unordered combinations (e.g. creates
#'   `foo(column_x, column_y)`, while `foo(column_y, column_x)` *will not* be created)
#'   - `"minimal` same as `"unique"` and further skips all self-matches (e.g.
#'   `foo(column_x, column_x)` *will not* be created)
#'
#' @returns
#' `across2()` returns a tibble with one column for each pair of elements in
#' `.xcols` and `.ycols` combined with each function in `.fns`.
#'
#' `across2x()` returns a tibble with one column for each combination between
#' elements in `.x` and`.y` combined with each function in `.fns`.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' For the basic functionality of `across()` please refer to the examples in
#' [`dplyr::across()`][dplyr::across].
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' `across2()` can be used to transfrom pairs of variables in one or more functions.
#' In the example below we want to calculate the product and the sum of all pairs of
#' 'Length' and 'Width' variables. We can use `{pre}` in the glue specification in
#' `.names` to extract the common prefix of each pair of variables. We can further
#' transform the names, in the example setting them `tolower` by specifying the
#' `.names_fn` argument:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   transmute(across2(ends_with("Length"),
#'                     ends_with("Width"),
#'                     .fns = list(product = ~ .x * .y,
#'                                 sum = ~ .x + .y),
#'                    .names = "{pre}_{fn}",
#'                    .names_fn = tolower))
#' ```
#'
#' `across2x()` can be used to perform calculations on each combination of variables.
#' In the example below we calculate the correlation between all variables in the
#' `iris` data set for each group. To do this, we `group_by` 'Species' and specify
#' the {tidyselect} helper `everything()` to `.xcols` and `.ycols`.
#' `~ round(cor(.x, .y), 2)` gives us the correlation rounded to two digits for each
#' pair of variables. We trim the rahter long variables names by replacing "Sepal"
#' with "S", and "Petal" with "P" in the `.names_fn` argument. Finally, we are not
#' interested in correlations of the same column and want to avoid excessive reults
#' by setting the `.comb` argument to `"minimal"`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across2x(everything(),
#'                      everything(),
#'                      ~ round(cor(.x, .y), 2),
#'                      .names_fn = ~ gsub("Sepal", "S", .x) %>%
#'                                      gsub("Petal", "P", .),
#'                      .comb = "minimal"))
#' ```
#'
#' @export
across2 <- function(.xcols, .ycols, .fns, ..., .names = NULL, .names_fn = NULL){

  setup <- meta_setup(dep_call = deparse_call(sys.call()),
                      setup_fn = "across2_setup",
                      xcols = rlang::enquo(.xcols),
                      ycols = rlang::enquo(.ycols),
                      fns = .fns,
                      names = .names,
                      names_fn = .names_fn)

  xvars <- setup$xvars
  yvars <- setup$yvars

  if (length(xvars) == 0L && length(yvars) == 0L) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

  .data <- dplyr::cur_data()

  xdata <- .data[xvars]
  ydata <- .data[yvars]

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


across2_setup <- function(xcols, ycols, fns, names, data, names_fn) {

  # clean last_value in setup_env
  if (exists("value", envir = .last)) {
    rm(value, envir = .last)
  }

  # setup: cols
  data <- dplyr::cur_data()[1, ]

  # setup error_output
  err_out <- list(data = data,
                  xcols = xcols,
                  ycols = ycols)

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
  is_glue <- any(grepl("{(xcol|ycol|fn|pre|suf|idx)}", names, perl = TRUE))

  # if .names use glue syntax:
  if (is_glue) {

    if (length(names) > 1) {
      rlang::abort(c("Problem with `across2()` input `.names`.",
                     i = "Glue specification must be a character vector of length == 1.",
                     x = paste0("`.names` is of length: ", length(names), ".")))
    }

    # setup index
    idx <- as.character(seq_len(vars_no))

    # setup pre and suf
    names2 <- names %||% ""
    pre1 <- NULL
    suf1 <- NULL

    # check pre and suf
    check_pre <- grepl("{pre}", names2, perl = TRUE)
    check_suf <- grepl("{suf}", names2, perl = TRUE)

    if (check_pre || check_suf) {

      if (is.function(fns) || rlang::is_formula(fns)) {
        names2 <- "{xcol}_{ycol}"
        fns <- list(`1` = fns)
      } else {
        names2 <- "{xcol}_{ycol}_{fn}"
      }

      var_nms <- purrr::flatten(purrr::map2(xvars, yvars, ~ list(c(.x, .y))))
      pre1 <- purrr::map(var_nms, ~ get_affix(.x, "prefix"))
      suf1 <- purrr::map(var_nms, ~ get_affix(.x, "suffix"))

      check_pre1 <- any(purrr::map_lgl(pre1, rlang::is_empty))
      check_suf1 <- any(purrr::map_lgl(suf1, rlang::is_empty))

      if (check_pre && check_pre1) {

        .last[["value"]] <- err_out

        rlang::abort(c("Problem with `across2()` input `.names`.",
                        i = "When `{pre}` is used inside `.names` each pair of input variables in `.xcols` and `.ycols` must share a common prefix of length > 0.",
                        x = "For at least one pair of variables a shared prefix could not be extracted.",
                        i = "Run `show_prefix()` to see the prefixes for each variable pair."))

      }
      pre1 <- unlist(pre1)

      if (check_suf && check_suf1) {

        .last[["value"]] <- err_out

        rlang::abort(c("Problem with `across2()` input `.names`.",
                       i = "When `{suf}` is used inside `.names` each pair of input variables in `.xcols` and `.ycols` must share a common suffix of length > 0.",
                       x = "For at least one pair of variables a shared suffix could not be extracted.",
                       i = "Run `show_suffix()` to see the suffixes for each variable pair."))
      }
      suf1 <- unlist(suf1)
    }

    names <- vctrs::vec_as_names(glue::glue(names,
                                            xcol = rep(xvars, each = length(fns)),
                                            ycol = rep(yvars, each = length(fns)),
                                            idx = idx,
                                            pre = rep(pre1, each = length(fns)),
                                            suf = rep(suf1, each = length(fns)),
                                            fn = rep(names_fns, length(xvars))),
                                 repair = "check_unique")

    # no correct glue syntax detected
  } else {
    # glue syntax might be wrong
    if (maybe_glue && length(names) == 1 && vars_no > 1) {
      rlang::abort(c("Problem with `across2()`  input `.names`.",
                     x = "Unrecognized glue specification `{...}` detected in `.names`.",
                     i = "`.names` only supports the following expressions: '{xcol}', '{ycol}', '{idx}' or '{fn}'."
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

#' @rdname across2
#' @export
across2x <- function(.xcols, .ycols, .fns, ..., .names = NULL, .names_fn = NULL, .comb = "all"){

  comb <- match.arg(.comb, c("all", "unique", "minimal"), several.ok = FALSE)

  setup <- meta_setup(dep_call = deparse_call(sys.call()),
                      setup_fn = "across2x_setup",
                      xcols = rlang::enquo(.xcols),
                      ycols = rlang::enquo(.ycols),
                      fns = .fns,
                      names = .names,
                      names_fn = .names_fn,
                      comb = comb)

  xvars <- setup$xvars
  yvars <- setup$yvars

  if (length(xvars) == 0L && length(yvars)) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  .data <- dplyr::cur_data()

  fns <- setup$fns
  names <- setup$names

  xdata <- .data[xvars]
  ydata <- .data[yvars]

  n_xcols <- length(xdata)
  n_ycols <- length(ydata)
  n_fns <- length(fns)
  seq_n_xcols <- seq_len(n_xcols)
  seq_n_ycols <- seq_len(n_ycols)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_xcols * n_ycols * n_fns)
  out_check <- vector("character", n_xcols * n_ycols * n_fns)

  for (i in seq_n_xcols) {
    x_nm <- names(xdata[i])
    xcol <- xdata[[i]]
    for(l in seq_n_ycols) {
      y_nm <- names(ydata[l])
      ycol <- ydata[[l]]
      new_nm <- paste(sort(c(x_nm, y_nm)), collapse = "_")
      if ((comb == "unique" || comb == "minimal") && new_nm %in% out_check) {
        k <- k + 1L
        out_check[k] <- new_nm
        next
      }
      out_check[k] <- new_nm
      if (comb == "minimal" && x_nm == y_nm) {
        k <- k + 1L
        next
      }
      for (j in seq_fns) {
        fn <- fns[[j]]
        out[[k]] <- fn(xcol, ycol, ...)
        k <- k + 1L
      }
    }
  }

  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  if (comb != "all" && length(.names) > 1 && setup$is_glue) { # check is is_glue is needed

    out <- purrr::compact(out)

    if (length(out) != length(names)) {
    rlang::abort(c("Problem with `across2x()` input `.names`.",
                   i = "The number of elements in `.names` must equal the number of new columns.",
                   x = paste0(length(.names), " elements provided to `.names`, but the number of new columns is ", length(out), ".")
    ))
    }
    names(out) <- names
  } else {
  names(out) <- names
  out <- purrr::compact(out)
  }
  tibble::new_tibble(out, nrow = size)
}


across2x_setup <- function(xcols, ycols, fns, names, cnames, data, names_fn, comb) {

  # setup: cols
  data <- dplyr::cur_data()[1, ]
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
  is_glue <- any(grepl("{(xcol|ycol|fn|idx)}", names, perl = TRUE))

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

    idx <- as.character(seq_len(vars_no))

    out <- vector("character", vars_no)

    for (i in seq_n_xcols) {
      for(l in seq_n_ycols) {
        for (j in seq_nm_fns) {
          out[[k]] <- glue::glue(names,
                                 xcol = xvars[[i]],
                                 ycol = yvars[[l]],
                                 idx = idx[[k]],
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
                     i = "`.names` only supports the following expressions: '{xcol}'. '{ycol}', '{idx}' or '{fn}'."
      ))
    }
    # check if non-glue names are unique
    vctrs::vec_as_names(names, repair = "check_unique")
    # check number of names
    if (comb == "all" && length(names) !=  vars_no) {
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

  value <- list(xvars = xvars, yvars = yvars, fns = fns, names = names, is_glue = is_glue)
  value
}
