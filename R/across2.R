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
across2 <- function(.cols1, .cols2, .fns, ..., .names = NULL){

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`across2()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep()

  setup <- across2_setup({{.cols1}},
                         {{.cols2}},
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

  # if (any(names %in% .cnames)) {
  #   dnames <- .cnames[.cnames %in% names]
  #   names_l <- ifelse(length(dnames) > 3, 3, length(dnames))
  #
  #   rlang::abort(c("Problem with `across2()`.",
  #                  i = "Output must not contain existing column names.",
  #                  x = paste0("`over()` tried to create the following existing column names: ",
  #                             paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
  #                             ifelse(length(dnames) > 3, " etc. ", ".")),
  #                  i = "If you want to transform existing columns try using `across()`.",
  #                  i = "If you want to change to output names use the `.names` argument"))
  #
  # }

  data1 <- dplyr::select(dplyr::cur_data(), dplyr::all_of(vars1))
  data2 <- dplyr::select(dplyr::cur_data(), dplyr::all_of(vars2))

  n_cols1 <- length(data1)
  n_fns <- length(fns)
  seq_n_cols1 <- seq_len(n_cols1)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_cols1 * n_fns)

  for (i in seq_n_cols1) {
    col1 <- data1[[i]]
    col2 <- data2[[i]]
    for (j in seq_fns) {
      fn <- fns[[j]]
      out[[k]] <- fn(col1, col2, ...)
      k <- k + 1L
    }
  }
  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


across2_setup <- function(cols1, cols2, fns, names, cnames, data) {

  cols1 <- rlang::enquo(cols1)
  cols2 <- rlang::enquo(cols2)
  # group_vars <- names(dplyr::cur_group())
  vars1 <- tidyselect::eval_select(rlang::expr(!!cols1), data) # check data
  vars2 <- tidyselect::eval_select(rlang::expr(!!cols2), data)
  vars1 <- names(vars1)
  vars2 <- names(vars2)

  # check later
  if (length(vars1) != length(vars2)) {
    rlang::abort(c("Problem with `across2()` input `.cols1` and `.cols2`.",
                   i = "Input `.cols1` and `.cols2` must use the same number of columns.",
                   x = paste0(length(vars1), "columns are selected in `.cols1`, ",
                              ", while ", length(vars2), " columns are selected in `.cols2`.")))
  }

  names2 <- names %||% ""
  pre1 <- NULL
  suf1 <- NULL

  if (stringr::str_detect(names2, "\\{pre\\}|\\{suf\\}")) {

    if (is.function(fns) || rlang::is_formula(fns)) {
      names2 <- "{col1}_{col2}"
      fns <- list(`1` = fns)
    } else {
      names2 <- "{col1}_{col2}_{fn}"
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

    # Instead construct list of lists with two elements var1 and var2
    # names2 <- vctrs::vec_as_names(glue::glue(names2,
    #                                         col1 = rep(vars1, each = length(fns)),
    #                                         col2 = rep(vars2, each = length(fns)),
    #                                         fn = rep(names_fns, length(cols1))),
    #                              repair = "check_unique")

    var_nms <- purrr::flatten(purrr::map2(vars1, vars2, ~ list(c(.x, .y))))
    pre1 <- purrr::map_chr(var_nms, ~ get_affix(.x, "right"))
    suf1 <- purrr::map_chr(var_nms, ~ get_affix(.x, "left"))

  }

  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{col1}_{col2}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{col1}_{col2}_{fn}"
  }

  if (!stringr::str_detect(names, "\\{pre\\}|\\{suf\\}")) {

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

  }

  names <- vctrs::vec_as_names(glue::glue(names,
                                          col1 = rep(vars1, each = length(fns)),
                                          col2 = rep(vars2, each = length(fns)),
                                          pre = rep(pre1, each = length(fns)),
                                          suf = rep(suf1, each = length(fns)),
                                          fn = rep(names_fns, length(cols1))),
                               repair = "check_unique")

  value <- list(vars1 = vars1, vars2 = vars2, fns = fns, names = names)
  value
}

# helper function for across2_setup
# To-Do: not only length == 1 but also only get alphanumeric / stop at punct
get_affix <- function(x, type = c("prefix", "suffix")) {

  side <- switch(type,
                 "prefix" = "right",
                 "suffix" = "left")

  x <- stringr::str_pad(x, max(nchar(x)), side = side, pad = " ")
  x_ls <- purrr::transpose(strsplit(x, ""))
  x_ls_length <- purrr::map_dbl(purrr::map(x_ls, unique), length)
  x_rle <- rle(x_ls_length)
  if (side == "right") {
    res <- stringr::str_sub(x[[1]],
                            start = 1L,
                            end = x_rle$length[1])

  } else {
    res_start <- sum(x_rle$length[-length(x_rle$length)])
    res_length <- x_rle$length[length(x_rle$length)]
    res_end <- res_start + res_length

    res <- stringr::str_sub(x[[1]],
                            start = res_start,
                            end = res_end)
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

# x <- c("Sepal.Length", "Sepal.Width")
# x <- c("Length.Sepal", "Width.Sepal")
# x <- c("Length.of.Sepal.here", "Length.no.Sepal.here")
# get_affix(x, "prefix")
