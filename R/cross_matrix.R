# To-Do: Allow more than one grouping variable
# To-Do: Add check: only inside dplyr summarise
# To-Do: Add check: data must not be grouped
# To-Do: Add print.method for function call in fn
# To-Do. Cleaner Names with .by
# To-Do: .out = c("wide", "long")


# cross_matrix <- function(.col, .fn, .by = NULL, .names = NULL) {
#
#   .data <- tryCatch({
#     dplyr::across()
#   }, error = function(e) {
#     rlang::abort("`cross_matrix()` must only be used inside dplyr verbs")
#   })
#
#   check_keep()
#
#   cols <- rlang::enquo(.col)
#   by <- rlang::enquo(.by)
#   vars <- tidyselect::eval_select(rlang::expr(!!cols), .data)
#   vars <- names(vars)
#
#   if (!is.null(.by)) {
#     if(length(vars) > 1) {
#       # To-Do: finalize
#       rlang::abort(c("When using `.by` only working with one variable"))
#     }
#
#     # To-Do: write without pipe
#     inp_x <- .data %>%
#       dplyr::select(!! cols, !! by) %>%
#       dplyr::nest_by(!! by) %>%
#       dplyr::mutate(data = setNames(list(data), !! by)) %>%
#       dplyr::ungroup() %>%
#       dplyr::pull(data)
#
#     grp_data <- dplyr::group_by(.data, !! by)
#
#     dplyr::summarise(grp_data,
#         crossXover_int(.cols = !! cols,
#                        .x = inp_x,
#                        .fns = .fn,
#                        .data = grp_data,
#                        .names = .names))
#
#   } else {
#
#     out <- across2x_int(.cols1 = vars,
#                         .cols2 = vars,
#                         .fns = .fn,
#                         .data = .data)
#
#     mtx <- matrix(unlist(out),
#                   nrow = length(vars),
#                   ncol = length(vars),
#                   dimnames = list(vars, vars))
#
#     tibble::as_tibble(mtx, rownames = "variable")
#   }
#
# }
