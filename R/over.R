#' Apply one or several functions to a vector in 'dplyr'
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
#' @param .vec An atomic vector (expect 'raw' and 'complex') to apply functions to.
#'   Instead of a vector a <[`selection helper`][selection_helpers]> or anything else
#'   that is coercible to an atomic vector can be used. Note that `over()` must only
#'   be used to create 'new' columns and will throw an error if `.vec` contains
#'   existing column names. To transform existing columns use [dplyr::across()].
#'
#' @param .fns Functions to apply to each of the elements in `.vec`. For
#'   functions that expect variable names as input, the selected strings need to
#'   be turned into symbols and evaluated. Note that <[`rlang's forcing operators`][rlang::nse-force]>
#'   do not work as expected in regular dplyr calls. See the examples below and the `vignette("over")`
#'   for `over()`'s genuine forcing function [`.()`].
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
#'   columns. This can use `{vec}` to stand for the selected vector element, and
#'   `{fn}` to stand for the name of the function being applied. The default
#'   (`NULL`) is equivalent to `"{vec}"` for the single function case and
#'   `"{vec}_{fn}"` for the case where a list is used for `.fns`.
#'
#' @returns
#' A tibble with one column for each element in `.vec` and each function in `.fns`;.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' `over()` can only be used inside `dplyr::mutate` or `dplyr::summarise`.
#' It has two main use cases. They differ in how the elements in `.vec`
#' are used. Let's first attach `dplyr`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' (1)
#' Here strings a supplied to `.vec` to construct column names (sharing the
#' same stem). This allows us to dynamically use more than one column in the
#' function calls in `.fns`. To work properly, the strings need to be
#' turned into symbols and evaluated. `over()`'s genuine forcing function
#' `.()` helps to declutter the otherwise rather verbose code. `.()` supports
#' glue syntax and takes a string as argument:
#'
#' Consider this example of a purrr-style formula in `.fns` with `.()`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(c("Sepal", "Petal"),
#'               ~ .("{.x}.Width") + .("{.x}.Length")
#'               ))
#' ```
#'
#' The above syntax is equal to the more verbose:
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(c("Sepal", "Petal"),
#'               ~ eval(sym(paste0(.x, ".Width"))) +
#'                 eval(sym(paste0(.x, ".Length")))
#'               ))
#' ```
#'
#' Note that `rlang`'s forcing operator `!!` is not supported inside `over()`.
#' ```{r, error = TRUE}
#' iris %>%
#'   mutate(over(c("Sepal", "Petal"),
#'               ~ !! sym(paste0(.x, ".Width")) +
#'                 !! sym(paste0(.x, ".Length"))
#'               ))
#' ```
#'
#' `.()` also works with anonymous functions
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   summarise(over(c("Sepal", "Petal"),
#'                 function(x) mean(.("{x}.Width"))
#'                 ))
#' ```
#'
#' A named list of functions
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(c("Sepal", "Petal"),
#'               list(product = ~ .("{.x}.Width") * .("{.x}.Length"),
#'                    sum = ~ .("{.x}.Width") + .("{.x}.Length"))
#'                    ),
#'          .keep = "none")
#' ```
#'
#' Use the `.names` argument to control the output names
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(c("Sepal", "Petal"),
#'               list(product = ~ .("{.x}.Width") * .("{.x}.Length"),
#'                    sum = ~ .("{.x}.Width") + .("{.x}.Length")),
#'               .names = "{fn}_{vec}"),
#'          .keep = "none")
#' ```
#'
#'
#' (2)
#' In the second use case the values in `.vec` are used as input
#' matched against conditions inside the functions in `.fns`.
#'
#' Lets create a dummy variable for each unique value in 'Species':
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(as.character(unique(Species)),
#'              ~ if_else(Species == .x, 1, 0)),
#'          .keep = "none")
#' ```
#'
#' `dist_values()` gets the ... :
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(dist_values(Species),
#'              ~ if_else(Species == .x, 1, 0)),
#'          .keep = "none")
#' ```
#'
#' #' `over()` also works on numeric variables, which is helpful to create several
#' dummy variables with different thresholds:
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#' mutate(over(seq(4, 7, by = 1),
#'             ~ if_else(Sepal.Length < .x, 1, 0),
#'             .names = "Sepal.Length_{vec}"),
#'          .keep = "none")
#' ```
#'
#' We can easily summarise the percent of each unique value of a variable:
#' ```{r, comment = "#>", collapse = TRUE}
#' mtcars %>%
#'   summarise(over(dist_values(gear),
#'                  ~ mean(gear == .x),
#'                  .names = "gear_{vec}"))
#' ```
#'
#' This is especially useful when working with grouped data. However, in this
#' case `dist_values()` should be called on factors, since it will require all
#' values to be present in all groups. If that is not the case it will through
#' an error.
#'
#' ```{r, error = TRUE}
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(over(dist_values(gear),
#'                  ~ mean(gear == .x)))
#' ```
#'
#' If used on a factor variable it will work:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' mtcars %>%
#'   mutate(gear = as.factor(gear)) %>%
#'   group_by(cyl) %>%
#'   summarise(over(dist_values(gear),
#'                  ~ mean(gear == .x),
#'                  .names = "gear_{vec}"))
#' ```
#'
#' @export
over <- function(.vec, .fns, ..., .names = NULL){

  data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`over()` must only be used inside dplyr verbs")
  })

  .cnames <- names(data)

  check_keep()

  setup <- over_setup({{ .vec }},
                      fns = .fns,
                      names = .names,
                      cnames = .cnames)

  vars <- setup$vars
  if (length(vars) == 0L) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }
  fns <- setup$fns
  names <- setup$names

  if (any(names %in% .cnames)) {
    dnames <- .cnames[.cnames %in% names]
    names_l <- ifelse(length(dnames) > 3, 3, length(dnames))

    rlang::abort(c("Problem with `over()` input `.vec`.",
                   i = "Input `.vec` must not contain existing column names.",
                   x = paste0("`.vec` contained the following column names: ",
                              paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
                              ifelse(length(dnames) > 3, " etc. ", ".")),
                   i = "If you want to transform existing columns try using `across()`."))

  }

  n_vec <- length(vars)
  n_fns <- length(fns)
  seq_n_vec <- seq_len(n_vec)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_vec * n_fns)

  for (i in seq_n_vec) {
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


over_setup <- function(vars, fns, names, cnames) {

  if(is.list(vars) && !rlang::is_named(vars)) {
    rlang::abort(c("Problem with `over()` input `.vec`.",
                   i = "If `.vec` is a list, it must be named.",
                   x = "`.vec` is an unnamed list."))
  }

  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{vec}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{vec}_{fn}"
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
                                          vec = rep(names(vars) %||% vars, each = length(fns)),
                                          fn = rep(names_fns, length(vars))),
                               repair = "check_unique")
  value <- list(vars = vars, fns = fns, names = names)
  value
}



