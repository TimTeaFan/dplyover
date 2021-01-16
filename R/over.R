#' Loop a vector or list over one or several functions in 'dplyr'
#'
#' @description
#' `over()` makes it easy to create new colums inside a [dplyr::mutate()] or
#' [dplyr::summarise()] call by applying a function (or a set of functions) to
#' an atomic vector or list using a syntax similar to [dplyr::across()].
#' The main difference is that [dplyr::across()] transforms or creates new columns
#' based on existing ones, while `over()` can only create new columns based on a
#' vector or list to which it will apply one or several functions. Whereas [dplyr::across()]
#' allows `tidy-selection` helpers to select columns, `over()` provides its own
#' helper functions to select strings or values based on either (1) values of
#' specified columns or (2) column names. See the examples below and the
#' `vignette("over")` for more details.
#'
#' @param .x An atomic vector or list to apply functions to. Alternatively a
#'   <[`selection helper`][selection_helpers]> can be used to create a vector.
#'
#' @param .fns Functions to apply to each of the elements in `.x`. For
#'   functions that expect variable names as input, the selected strings need to
#'   be turned into symbols and evaluated. `dplyrover` comes with a genuine helper
#'   function that evaluates strings as names [`.()`]. Note that <[`rlang's forcing operators`][rlang::nse-force]>
#'   are not supported in `over()`.
#'
#'   Possible values are:
#'
#'   - A function
#'   - A purrr-style lambda
#'   - A list of functions/lambdas
#'
#'   For examples see the example section below.
#'
#'   Note that, unlike `across()`, `over()` does not accept `NULL` as a
#'   value to `.fns`.
#'
#' @param ... Additional arguments for the function calls in `.fns`.
#'
#' @param .names A glue specification that describes how to name the output
#'   columns. This can use `{x}` to stand for the selected vector element, and
#'   `{fn}` to stand for the name of the function being applied. The default
#'   (`NULL`) is equivalent to `"{x}"` for the single function case and
#'   `"{x}_{fn}"` for the case where a list is used for `.fns`.
#'
#'   Note that, depending on the nature of the underlying object in `.x`,
#'   specifying `{x}` will yield different results:
#'
#'   - If `.x` is an unnamed atomic vector, `{x}` will represent each value.
#'   - If `.x` is a named list or atomic vector, `{x}` will represent each name.
#'   - If `.x` is an unnamed list, `{x}` will be the index number running from 1 to `length(x)`.
#'
#'   This standard behavior (interpretation of `{x}`) can be overwritten by
#'   directly specifying:
#'
#'   - `{x_val}` for `.x`'s values
#'   - `{x_nm}` for its names
#'   - `{x_idx}` for its index numbers
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
#' A tibble with one column for each element in `.x` and each function in `.fns`.
#'
#' @section Note:
#' `over()` must only be used to create 'new' columns and will throw an error if
#' the new columns created contain existing column names. To transform existing
#' columns use [dplyr::across()]¥.
#'
#' Similar to `dplyr::across()` `over()` works only inside dplyr verbs.
#'
#' @seealso
#' [over2()] to apply a function to two objects.
#'
#' All members of the <[`over-across function family`][over_across_family]>.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
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
#'
#' ## (1) The General Use Case
#' Here the values in `.x` are used as inputs to one or more functions in `.fns`.
#' This is useful, when we want to create several new variables based on the same
#' function with varying arguments. A good example is creating a bunch of lagged
#' variables.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tibble(x = 1:25) %>%
#'   mutate(over(c(1:3),
#'               ~ lag(x, .x)))
#' ```
#'
#' Lets create a dummy variable for each unique value in 'Species':
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(unique(Species),
#'              ~ if_else(Species == .x, 1, 0)),
#'          .keep = "none")
#' ```
#' Note that `dplyover` comes with a helper function similar to `unique` called
#' [`dist_values()`] which will handle `NA`s differently.
#'
#' With `over()` it is also possible to create several dummy variables with
#' different thresholds. We can use the `.names` argument to control the output
#' names:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#' mutate(over(seq(4, 7, by = 1),
#'             ~ if_else(Sepal.Length < .x, 1, 0),
#'             .names = "Sepal.Length_{x}"),
#'          .keep = "none")
#' ```
#'
#' A similar approach can be used with dates. Below we loop over a date
#' sequence to check whether the date falls within a given start and end
#' date. We can use the `.names_fn` argument to clean the resulting output
#' names:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # some dates
#' dat_tbl <- tibble(start = seq.Date(as.Date("2020-01-01"),
#'                                    as.Date("2020-01-15"),
#'                                    by = "days"),
#'                   end = start + 10)
#'
#' dat_tbl %>%
#'   mutate(over(seq(as.Date("2020-01-01"),
#'                   as.Date("2020-01-21"),
#'                   by = "weeks"),
#'               ~ .x >= start & .x <= end,
#'               .names = "day_{x}",
#'               .names_fn = ~ gsub("-", "", .x)))
#' ```
#'
#' `over()` can summarise data in wide format. In the example below, we want to
#' know for each group of customers (`new`, `existing`, `reactivate`), how much
#' percent of the respondents gave which rating on a five point likert scale (`item1`).
#' A usual approach in the tidyverse would be to use `count %>% group_by %>% mutate`,
#' which yields the same result in the usually prefered long format. Sometimes, however,
#' we might want this kind of summary in the wide format, and in this case `over()`
#' comes in handy:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' csatraw %>%
#'   group_by(type) %>%
#'   summarise(over(c(1:5),
#'                  ~ mean(item1 == .x)))
#' ```
#'
#' Instead of a vector we can provide a named list of vectors to calculate the
#' top two and bottom two categories on the fly:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' csatraw %>%
#'   group_by(type) %>%
#'   summarise(over(list(bot2 = c(1:2),
#'                       mid  = 3,
#'                       top2 = c(4:5)),
#'                  ~ mean(item1 %in% .x)))
#' ```
#'
#' `over()` can also loop over columns of a data.frame. In the example below we
#' want to create four different dummy variables out  `item1`: (i) the top and (ii)
#' bottom category as well as (iii) the top two and (iv) the bottom two categories.
#' We can create a lookup data.frame and use all columns but the first as input to
#' `over()`. In the function call we make use of base R's `match()`, where `.x`
#' represents the new values and `recode_df[, 1]` refers to the old values.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#'
#' recode_df <- data.frame(old  = c(1, 2, 3, 4, 5),
#'                         top1 = c(0, 0, 0, 0, 1),
#'                         top2 = c(0, 0, 0, 1, 1),
#'                         bot1 = c(1, 0, 0, 0, 0),
#'                         bot2 = c(1, 1, 0, 0, 0))
#'
#' csatraw %>%
#'   mutate(over(recode_df[,-1],
#'               ~ .x[match(item1, recode_df[, 1])],
#'               .names = "item1_{x}")) %>%
#'   select(starts_with("item1"))
#' ```
#'
#' `over()` does also work with list-columns. In the example below, the colum
#' `csat_open` contains one or more reasons why a specific Customer Satisfaction
#' Rating was given. We can easily create a column for each response category:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' csat %>%
#'   mutate(over(unique(unlist(csat_open)),
#'               ~ as.integer(grepl(.x, csat_open)),
#'               .names = "rsp_{x}",
#'               .names_fn = ~ gsub("\\s", "_", .x)))
#' ```
#'
#'
#' ## (2) A Very Specific Use Case
#' Here strings are supplied to `.x` to construct column names (sharing the
#' same stem). This allows us to dynamically use more than one column in the
#' function calls in `.fns`. To work properly, the strings need to be
#' turned into symbols and evaluated. For this `dplyover` provides a genuine helper
#' function `.()` that evaluates strings and helps to declutter the otherwise rather
#' verbose code. `.()` supports glue syntax and takes a string as argument:
#'
#' Consider the following example of a purrr-style formula in `.fns` using `.()`:
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
#' `.()` also works with anonymous functions:
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   summarise(over(c("Sepal", "Petal"),
#'                 function(x) mean(.("{x}.Width"))
#'                 ))
#' ```
#'
#' A named list of functions:
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(c("Sepal", "Petal"),
#'               list(product = ~ .("{.x}.Width") * .("{.x}.Length"),
#'                    sum = ~ .("{.x}.Width") + .("{.x}.Length"))
#'                    ),
#'          .keep = "none")
#' ```
#'
#' Again, use the `.names` argument to control the output names:
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(c("Sepal", "Petal"),
#'               list(product = ~ .("{.x}.Width") * .("{.x}.Length"),
#'                    sum = ~ .("{.x}.Width") + .("{.x}.Length")),
#'               .names = "{fn}_{x}"),
#'          .keep = "none")
#' ```
#' @export
over <- function(.x, .fns, ..., .names = NULL, .names_fn = NULL){

  deparse_call <- deparse(sys.call(),
                          width.cutoff = 500L,
                          backtick = TRUE,
                          nlines = 1L,
                          control = NULL)

  # get group id for setup
  grp_id <- tryCatch({
    dplyr::cur_group_id()
  }, error = function(e) {
    rlang::abort("`over()` must only be used inside dplyr verbs")
  })

# meta setup
  par_frame <- parent.frame()
  setup_exists <- exists(".__dplyover_setup__.", envir = par_frame)

  # if setup already exists
  if (setup_exists && grp_id > 1L) {
    # get data
    meta_setup <- get(".__dplyover_setup__.", envir = par_frame)
    # get call number
    call_no <- which.min(meta_setup$call_his)
    call_id <- paste0("call", call_no)
    # update "call_his"
    par_frame[[".__dplyover_setup__."]][["call_his"]][call_no] <- grp_id
    # check call
    if (!identical(meta_setup$call_lang[call_no], deparse_call)) {
      rlang::abort(c("Problem with the way `over()` was called.",
                     i = "It seems like different calls to `over()` were made depending on the group_id.",
                     x = "For technical reasons, it is not possible that the call to `over()` depends on the group_id."))
    }
    # get data from existing call
    .cnames <- meta_setup[[call_id]]$cnames
    setup <- meta_setup[[call_id]]$setup

  # if this is a new call to over: update par_frame
  } else {
    if (!setup_exists) {
    # new setup
      check_keep(type = "keep")
      par_frame[[".__dplyover_setup__."]][["call_his"]] <- grp_id
      par_frame[[".__dplyover_setup__."]][["call_lang"]] <- deparse_call
      call_id <- paste0("call", 1)

    # existing setup, but new call
    } else {
      meta_setup <- get(".__dplyover_setup__.", envir = par_frame)
      # register new call
      par_frame[[".__dplyover_setup__."]][["call_his"]] <- c(meta_setup$call_his, 1)
      par_frame[[".__dplyover_setup__."]][["call_lang"]] <- c(meta_setup$call_lang, deparse_call)
      # get number of current call
      call_id <- paste0("call", which.min(meta_setup$call_his))
    }

    # write data into parent frame
    .data <-  dplyr::cur_data()
    par_frame[[".__dplyover_setup__."]][[call_id]][["cnames"]] <- .cnames <- names(.data)
    par_frame[[".__dplyover_setup__."]][[call_id]][["setup"]] <- setup <- over_setup(
      .x,
      fns = .fns,
      names = .names,
      cnames = .cnames,
      names_fn = .names_fn)

    # checks
    if (length(setup$x) == 0L) {
      return(tibble::new_tibble(list(), nrow = 1L))
    }

    if (any(setup$names %in% .cnames)) {
      dnames <- .cnames[.cnames %in% setup$names]
      names_l <- ifelse(length(dnames) > 3, 3, length(dnames))

      rlang::abort(c("Problem with `over()`.",
                     i = "Output must not contain existing column names.",
                     x = paste0("`over()` tried to create the following existing column names: ",
                                paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
                                ifelse(length(dnames) > 3, " etc. ", ".")),
                     i = "If you want to transform existing columns try using `dplyr::across()`.",
                     i = "If you want to change the output names use the `.names` argument."))
    }
  }

  # actual function starts here
  x <- setup$x
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
    for (j in seq_fns) {
      fn <- fns[[j]]
      out[[k]] <- fn(xi, ...)
      k <- k + 1L
    }
  }
  size <- vctrs::vec_size_common(!!!out)
  out <- vctrs::vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  tibble::new_tibble(out, nrow = size)
}


over_setup <- function(x1, fns, names, cnames, names_fn) {

  # setup name variants
  x1_nm <- names(x1)
  x1_idx <- as.character(seq_along(x1))
  x1_val <- if (is.data.frame(x1) && nrow(x1) != 1) {
    NULL
  } else if (is.list(x1) && is.vector(x1) &&
             any(purrr::map_lgl(x1, ~ length(.x) != 1))) {
    NULL
  } else {
    x1
  }

  # apply `.names` smart default
  if (is.function(fns) || rlang::is_formula(fns)) {
    names <- names %||% "{x}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{x}_{fn}"
  }

  if (!is.list(fns)) {
    rlang::abort(c("Problem with `over()` input `.fns`.",
            i = "Input `.fns` must be a function, a formula, or a list of functions/formulas."))
  }

  # use index for unnamed lists
  if (is.list(x1) && !rlang::is_named(x1)) {
    names(x1) <- x1_idx
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
  is_glue <- any(grepl("{(x|x_val|x_nm|x_idx|fn)}", names, perl = TRUE))

  # if .names use glue syntax:
  if (is_glue) {

    if (length(names) > 1) {
      rlang::abort(c("Problem with `over()` input `.names`.",
                     i = "Glue specification must be a character vector of length == 1.",
                     x = paste0("`.names` is of length: ", length(names), ".")))
    }

    # warn that default values are used if conditions not met
    if (is.null(x1_val) && grepl("{x_val}", names, perl = TRUE)) {
      rlang::warn("in `over()` `.names`: used 'x_idx' instead of 'x_val'. The latter only works with lists if all elements are length 1.")
    }

    if (is.null(x1_nm) && grepl("{x_nm}", names, perl = TRUE)) {
      rlang::warn("in `over()` `.names`: used 'x_idx' instead of 'x_nm', since the input object is unnamed.")
    }

    names <- vctrs::vec_as_names(glue::glue(names,
                                            x = rep(names(x1) %||% x1, each = length(fns)),
                                            x_val = rep(x1_val %||% x1_idx, each = length(fns)),
                                            x_nm = rep(x1_nm %||% x1_idx, each = length(fns)),
                                            x_idx = rep(x1_idx, each = length(fns)),
                                            fn = rep(names_fns, length(x1))),
                                 repair = "check_unique")

  # no correct glue syntax detected
  } else {
    # glue syntax might be wrong
    if (maybe_glue && length(names) == 1 && vars_no > 1) {
      rlang::abort(c("Problem with `over()`  input `.names`.",
                     x = "Unrecognized glue specification `{...}` detected in `.names`.",
                     i = "`.names` only supports the following expressions: '{x}', '{x_val}', '{x_nm}', '{x_idx}' or '{fn}'."
      ))
    }
    # check that non-glue names are unique
    vctrs::vec_as_names(names, repair = "check_unique")
    # check number of names
    if (length(names) !=  vars_no) {
      rlang::abort(c("Problem with `over()`  input `.names`.",
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



  value <- list(x = x1, fns = fns, names = names)
  value
}



