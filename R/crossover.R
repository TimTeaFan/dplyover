#' Loop columns and a vector over one or several functions in 'dplyr'
#'
#' @description
#' `crossover()` and `crossoverx()` combine the functionality of [dplyr::across()]
#' with [over()] by iterating over (i) one or more columns (`.xcols`) and,
#' simultaneously, (ii) elements of a vector or list (`.y`). Regardings its first
#' two inputs `crossover()` is basically a simple loop, while `crossoverx()` is
#' a nested loop, which creates each combination of all columns in `.xcols` and
#' elements in `.y`.
#'
#' `crossover()` has one trick up it's sleeves: Its second input
#' (`.y`) can be a function. This changes the originial behavior slightly: First
#' the function in `.y` is applied to all columns in `.xcols` to *generate* a
#' vector or list which will be used as `.y` input. Then each column is looped
#' over all the outputs that it generated with the function supplied to `.y`.
#' For examples see the example section below.
#'
#' @param .xcols <[`tidy-select`][dplyr_tidy_select]> Columns to transform.
#'   Because `crossover()` is used within functions like `summarise()` and
#'   `mutate()`, you can't select or compute upon grouping variables.
#'
#' @param .y An atomic vector or list to apply functions to. `crossover()` also
#'   accepts a function as `.y` argument. In this case each column in `.xcols`
#'   is looped over all the outputs that it generated with the function supplied
#'   to `.y`.
#'
#'   If a function is supplied the following values are possible:
#'
#'   - A bare function name, e.g. `mean`
#'   - An anonymous function, e.g. `function(x) mean(x)`
#'   - A purrr-style lambda, e.g. `~ mean(.x, na.rm = TRUE)`
#'
#'   Note that additional arguments can only be specified with an anonymous
#'   function, a purrr-style lamba or with a pre-filled custom function.
#'
#' @param .fns Functions to apply to each column in `.xcols` and element in `.y`.
#'   Note that <[`rlang's forcing operators`][rlang::nse-force]> are not
#'   supported in `crossover()`.
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
#' `crossover()` returns a tibble with one column for each pair of columns in `.xcols`
#' and elements in `.y` combined with each function in `.fns`.
#'
#' If a function is supplied as `.y` argument, `crossover()` returns a tibble with
#' one column for each pair of output elements of `.y` and the column in `.xcols`
#' that generated the output combined with each function in `.fns`.
#'
#' `crossoverx()` returns a tibble with one column for each each combination of
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
#' Here is a simple toy example of `crossover()`. The first argument selects
#' all columns starting with "Sepal" (here: 'Sepal.Length' and 'Sepal.Width').
#' The  second argument is a vector of `5` and `3`. The function `.x <= .y` is
#' applied to each pair of colums and elements, where `.x` stands for the column in
#' `.xcols` and `.y` for the element in `.y`. In this case two `logical` columns
#' are created: 'Sepal.Length_5' shows whether 'Sepal.Length' is equal or smaller
#' than 5, and 'Sepal.Width_3' shows whether 'Sepal.Length' is equal or smaller
#' than 3.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#'  iris %>%
#'    mutate(crossover(starts_with("Sepal"),
#'                     c(5,3),
#'                      ~ .x <= .y)) %>%
#'    glimpse
#' ```
#'
#' The `.y` argument of `crossover()` can take a function instead of list or vector.
#' In the example below we select the columns 'type', 'product', 'csat' in `.xcols`.
#' We supply the function [dist_values()] to `.y`, which is a cleaner variant of
#' base R's `unique()`. This generates all distinct values for all three selected
#' variables. Now, the function in `.fns`, `~ if_else(.y == .x, 1, 0)`, is applied
#' to each pair of distinct value in `.y` and the column in `.xcols` that generated
#' this value. This basically creates a dummy variable each value of each variable.
#' Since some of the values contain space characters, we can use the `.names_fn`
#' argument to supply a *third* function that cleans the output names by replacing
#' spaces with an underscore and setting all characters `tolower()`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#'  csat %>%
#'    transmute(
#'      crossover(.xcols = c(type, product, csat),
#'                .y = dist_values,
#'                .fns = ~ if_else(.y == .x, 1, 0),
#'                .names_fn = ~ gsub("\\s", "_", .x) %>% tolower(.)
#'                )) %>%
#'    glimpse
#' ```
#'
#' `crossoverx()` loops every combination of colums in `.xcols` and elements
#' `.y` over the functions in `.fns`. This is helpful in cases where we want to
#' create a batch of similar variables with only slightly changes in the arguments
#' of the calling function. A good example are lagged variables. Below we create
#' five lagged variables for each 'Sepal.Length' and  'Sepal.Width'. To create
#' nice names we use a named list as argument in `.fns` and specify the glue
#' syntax in `.names`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#'  iris %>%
#'    transmute(
#'      crossoverx(starts_with("sepal"),
#'                 1:5,
#'                 list(lag = ~ lag(.x, .y)),
#'                 .names = "{xcol}_{fn}{y}")) %>%
#'    glimpse
#' ```
#'
#' @export
crossover <- function(.xcols = dplyr::everything(), .y, .fns, ..., .names = NULL, .names_fn = NULL){

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`crossover()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep()

  setup <- crossover_setup({{.xcols}},
                       .y,
                       fns = .fns,
                       names = .names,
                       cnames = .cnames,
                       data = .data,
                       names_fn = .names_fn)

  vars <- setup$vars
  y <- setup$y

  if (length(vars) == 0L) {
    return(tibble::new_tibble(list(), nrow = 1L))
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
                   i = "If you want to change the output names use the `.names` argument."))

  }

  if (setup$each) {
    data <- dplyr::select(dplyr::cur_data(), dplyr::all_of(unique(vars)))
    data_ls <- as.list(data)[vars]
    data <- tibble::new_tibble(data_ls, nrow = nrow(data))
  } else {
    data <- dplyr::select(dplyr::cur_data(), dplyr::all_of(vars))
  }

  n_cols <- length(data)
  n_fns <- length(fns)
  seq_n_cols <- seq_len(n_cols)
  seq_fns <- seq_len(n_fns)

  k <- 1L
  out <- vector("list", n_cols * n_fns)

  for (i in seq_n_cols) {
    col <- data[[i]]
    yi <- y[[i]]
    for (j in seq_fns) {
      fn <- fns[[j]]
      out[[k]] <- fn(col, yi, ...)
      k <- k + 1L
    }
  }

  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


crossover_setup <- function(cols, y1, fns, names, cnames, data, names_fn, each = FALSE) {

  # setup: cols
  cols <- rlang::enquo(cols)
  # TODO: Check if needed:
  # cols <- rlang::quo_set_env(cols,
  #                            data_mask_top(rlang::quo_get_env(cols),
  #                                          recursive = FALSE,
  #                                          inherit = TRUE))
  # vars <- tidyselect::eval_select(cols, data)
  vars <- tidyselect::eval_select(rlang::expr(!!cols), data)
  vars <- init_vars <- names(vars)

  # setup: .y

  # if .y is function:
  if (is.function(y1) || rlang::is_formula(y1)) {

    # expand vars
    y1 <- rlang::as_function(y1)
    y1 <- purrr::map(dplyr::select(data, !! cols), y1)
    vars <- unlist(purrr::imap(y1, ~ rep(.y, length(.x))))
    y1 <- unlist(y1, recursive = FALSE)
    if (!is.list(y1)) y1 <- unname(y1)

    # set flag `each` if neccesary
    if (length(vars) > length(init_vars)) {
      each <- TRUE
    }
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

  # check lengths
  if (length(vars) != length(y1)) {
    rlang::abort(c("Problem with `crossover()` inputs `.xcols` and `.y`.",
                   i = "The number of `.xcols` must be equal to the number of elements in  `.y`.",
                   x = paste0(length(vars), " columns are selected in `.xcols`, ",
                              "while `.y` contains ", length(y1), " elements.")))
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

  # TODO: Default needed when function in .y returns values?

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
  vars_no <- length(y1) * length(fns)
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

    names <- vctrs::vec_as_names(glue::glue(names,
                                            xcol = rep(vars, each = length(fns)),
                                            y = rep(names(y1) %||% y1, each = length(fns)),
                                            y_val = rep(y1_val %||% y1_idx, each = length(fns)),
                                            y_nm = rep(y1_nm %||% y1_idx, each = length(fns)),
                                            y_idx = rep(y1_idx, each = length(fns)),
                                            fn = rep(names_fns, length(y1))),
                                 repair = "check_unique")

    # no correct glue syntax detected
  } else {
    # glue syntax might be wrong
    if (maybe_glue && length(names) == 1 && vars_no > 1) {
      rlang::abort(c("Problem with `crossover()`  input `.names`.",
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

  value <- list(vars = vars, y = y1, fns = fns, names = names, each = each)
  value
}

#' @rdname crossover
#' @export
crossoverx <- function(.xcols = dplyr::everything(), .y, .fns, ..., .names = NULL, .names_fn = NULL){

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`crossoverx()` must only be used inside dplyr verbs")
  })

  .cnames <- names(.data)

  check_keep()

  setup <- crossoverx_setup({{.xcols}},
                           .y,
                           fns = .fns,
                           names = .names,
                           cnames = .cnames,
                           data = .data,
                           names_fn = .names_fn)

  vars <- setup$vars
  y <- setup$y

  if (length(vars) == 0L) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }

  fns <- setup$fns
  names <- setup$names

  if (any(names %in% .cnames)) {
    dnames <- .cnames[.cnames %in% names]
    names_l <- ifelse(length(dnames) > 3, 3, length(dnames))

    rlang::abort(c("Problem with `crossoverx()`.",
                   i = "Output must not contain existing column names.",
                   x = paste0("`crossover()` tried to create the following existing column names: ",
                              paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
                              ifelse(length(dnames) > 3, " etc. ", ".")),
                   i = "If you want to transform existing columns try using `across()`.",
                   i = "If you want to change the output names use the `.names` argument."))

  }

  data <- dplyr::select(dplyr::cur_data(), dplyr::all_of(vars))

  n_cols <- length(data)
  n_y <- length(y)
  n_fns <- length(fns)
  seq_n_cols <- seq_len(n_cols)
  seq_n_y <- seq_len(n_y)
  seq_fns <- seq_len(n_fns)

  k <- 1L
  out <- vector("list", n_cols * n_y * n_fns)

  for (i in seq_n_cols) {
    col <- data[[i]]
    for(l in seq_n_y) {
      yl <- y[[l]]
      for (j in seq_fns) {
        fn <- fns[[j]]
        out[[k]] <- fn(col, yl, ...)
        k <- k + 1L
      }
    }
  }

  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


crossoverx_setup <- function(cols, y1, fns, names, cnames, data, names_fn) {

  # setup: cols
  cols <- rlang::enquo(cols)
  # TODO: Check if needed:
  # cols <- rlang::quo_set_env(cols,
  #                            data_mask_top(rlang::quo_get_env(cols),
  #                                          recursive = FALSE,
  #                                          inherit = TRUE))
  # vars <- tidyselect::eval_select(cols, data)
  vars <- tidyselect::eval_select(rlang::expr(!!cols), data)
  vars <- init_vars <- names(vars)

  # setup: .y
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
    rlang::abort(c("Problem with `crossoverx()` input `.fns`.",
                   i =  "Input `.fns` must be a function, a formula, or a list of functions/formulas."))
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
  vars_no <- length(vars) * length(y1) * length(fns)
  maybe_glue <- any(grepl("{.*}", names, perl = TRUE))
  is_glue <- any(grepl("{(xcol|y|y_val|y_nm|y_idx|fn)}", names, perl = TRUE))

  # if .names use glue syntax:
  if (is_glue) {

    if (length(names) > 1) {
      rlang::abort(c("Problem with `crossoverx()` input `.names`.",
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
      rlang::abort(c("Problem with `crossoverx()`  input `.names`.",
                     x = "Unrecognized glue specification `{...}` detected in `.names`.",
                     i = "`.names` only supports the following expressions: '{xcol}'. '{y}', '{y_val}', '{y_nm}', '{y_idx}' or '{fn}'."
      ))
    }
    # check if non-glue names are unique
    vctrs::vec_as_names(names, repair = "check_unique")
    # check number of names
    if (length(names) !=  vars_no) {
      rlang::abort(c("Problem with `crossoverx()`  input `.names`.",
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


crossXover_int <- function(.cols, .x, .fns, .data, ..., .names = NULL){

  .cnames <- names(.data)

  check_keep()

  setup <- crossXover_setup({{.cols}},
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


