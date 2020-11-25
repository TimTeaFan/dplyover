# To-Do: solve argument passing of cols to inner function
pair_matrix <- function(.cols, .fn) {

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`grouped_matrix()` must only be used inside dplyr verbs")
  })

  check_keep()

  cols <- rlang::enquo(.cols)
  vars <- tidyselect::eval_select(rlang::expr(!!cols), .data)
  vars <- names(vars)


  out <- across2x(.cols1 = vars,
                  .cols2 = vars,
                  .fns = .fn)

  mtx <- matrix(unlist(out),
           nrow = length(vars),
           ncol = length(vars),
           dimnames = list(vars, vars))

  tibble::as_tibble(mtx, rownames = "variable")

}

# To-Do: Allow more than one grouping variable
# To-Do: Add check: only inside dplyr summarise
# To-Do: Add check: data must not be grouped
grouped_matrix <- function(.col, .by, .fn, .names = NULL) {

  .data <- tryCatch({
    dplyr::across()
  }, error = function(e) {
    rlang::abort("`grouped_matrix()` must only be used inside dplyr verbs")
  })

  check_keep()

  inp_x <- .data %>%
    dplyr::select({{.col}}, {{.by}}) %>%
    dplyr::nest_by({{.by}}) %>%
    dplyr::mutate(data = setNames(list(data), {{.by}})) %>%
    dplyr::ungroup() %>%
    dplyr::pull(data)

  grp_data <- dplyr::group_by(.data, {{.by}})

  if (is.null(.names)) {
    dplyr::summarise(grp_data,
    crossXover(.cols = {{.col}},
              .x = inp_x,
              .fns = .fn))
  } else {
    dplyr::summarise(grp_data,
                     crossXover(.cols = {{.col}},
                                .x = inp_x,
                                .fns = .fn,
                                .names = .names))
  }

}
