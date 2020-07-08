#' Apply one or several functions to a character vector in 'dplyr'
#'
#' @description
#' `over()` makes it easy to create new colums inside a [dplyr::mutate()] or
#' [dplyr::summarise()] call by applying a function (or a set of functions) to
#' a character vector using a syntax similar to the existing [dplyr::across()].
#' The main difference is that [dplyr::across()] transforms or creates new
#' columns based on existing ones, while `over()` creates new columns based on a
#' character vector to which it will apply one or several functions. Whereas
#' [dplyr::across()] allows `tidy-selection` helpers to select columns,
#' `over()` provides its own helper functions to select strings based on either
#' (1) column names or (2) values of specified columns. See `vignette("over")`
#' and `vignette("string_selection_helpers")` for more details.
#'
#' @param .strs A character vector to apply functions to. Instead of a character
#'   vector a <[`string selection helper`][string_selection_helpers]> or any other function
#'   that evaluates to a character vector can be used. Note that `over()` must
#'   only be used to create 'new' columns and will throw an error if `.strs`
#'   contains existing column names. To transform existing columns use [dplyr::across()].
#'
#' @param .fns Functions to apply to each of the selected strings. Note that for
#'   functions that expect variable names as input, the selected strings need to
#'   be turned into symbols and evaluated early using <[`rlang's forcing operators`][rlang::nse-force]>.
#'
#'   Possible values are:
#'
#'   - A function
#'   - A purrr-style lambda
#'   - A list of functions/lambdas
#'
#'   For examples see below.
#'
#'   Note that, unlike `across()`, `over()` does not accept `NULL` as a
#'   value to `.fns``.
#'
#' @param ... Additional arguments for the function calls in `.fns`.
#'
#' @param .names A glue specification that describes how to name the output
#'   columns. This can use `{str}` to stand for the selected string name, and
#'   `{fn}` to stand for the name of the function being applied. The default
#'   (`NULL`) is equivalent to `"{str}"` for the single function case and
#'   `"{str}_{fn}"` for the case where a list is used for `.fns`.
#'
#' @returns
#' A tibble with one column for each string in `.strs` and each function in `.fns`.
#' @examples
#' # over() -----------------------------------------------------------------
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean))
#' iris %>%
#'   as_tibble() %>%
#'   mutate(across(where(is.factor), as.character))
#'
#' # A purrr-style formula
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), ~mean(.x, na.rm = TRUE)))
#'
#' # A named list of functions
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd)))
#'
#' # Use the .names argument to control the output names
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean, .names = "mean_{col}"))
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd), .names = "{col}.{fn}"))
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean, sd), .names = "{col}.fn{fn}"))
#' @export
over <- function(.strs, .fns = NULL, ..., .names = NULL){

  data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`over()` must only be used inside dplyr verbs")
  })

  setup <- over_setup({{ .strs }},
                      fns = .fns,
                      names = .names,
                      cnames = names(data))

  vars <- setup$vars
  if (length(vars) == 0L) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }
  fns <- setup$fns
  names <- setup$names

  check_keep()

  n_strs <- length(vars)
  n_fns <- length(fns)
  seq_n_strs <- seq_len(n_strs)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_strs * n_fns)

  for (i in seq_n_strs) {
    # var <- vars[[i]]
    str <- vars[[i]]
    for (j in seq_fns) {
      fn <- fns[[j]]
      out[[k]] <- fn(str, ...)
      k <- k + 1L
    }
  }
  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


over_setup <- function(strs, fns, names, cnames) {

  if(!is.character(strs)) {
    rlang::abort(c("Problem with `over()` input `.strs`.",
            i = "Input `.strs` must be a character vector or a function that evaluates to a character vector"))
  } else {
    vars <- strs
  }
  if (any(vars %in% cnames)) {
    dnames <- cnames[cnames %in% vars]
    names_l <- ifelse(length(dnames) > 3, 3, length(dnames))

    rlang::abort(c("Problem with `over()` input `.strs`.",
            i = paste0("Input `.strs` must not contain existing column names like ",
                       paste0(paste0("`", dnames[seq_along(1:names_l)], "`"), collapse = ", "),
                       ifelse(length(dnames) > 3, " etc. ", ".")
                       ),
            "If you want to transform existing columns try using `across()`."))

  }
  if (is.null(fns)) {
    rlang::abort(c("Problem with `over()` input `.fns`.",
            i = "Input `.fns` must be a function or a list of functions.",
            "Try using `across(), if you want to return the data untransformed."))

  }
  # account for named character vectors to overwrite .names argument
  if (is.function(fns) || is_formula(fns)) {
    names <- names %||% "{str}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{str}_{fn}"
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

  names <- vctrs::vec_as_names(glue::glue(names, str = rep(vars, each = length(fns)),
                                          fn = rep(names_fns, length(vars))),
                               repair = "check_unique")
  value <- list(vars = vars, fns = fns, names = names)
  value
}
