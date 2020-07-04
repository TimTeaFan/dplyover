`.` <- function(x) {
  rlang::eval_tidy(rlang::sym(glue::glue(x,
                                         .open = "{{",
                                         .close = "}}",
                                         .envir = parent.frame())),
                   env = rlang::caller_env())
}

over_setup <- function(strs, fns, names) {

  if(!is.character(vars)) {
    abort(c("Problem with `over()` input `.strs`.",
            i = "Input `.strs` must be character values or a function that evaluates to character values."))
  } else {
    vars <- strs
  }

  if (is.null(fns)) {
    abort(c("Problem with `over()` input `.fns`.",
            i = "Input `.fns` must be a function or a list of functions."))

  }

  if (is.function(fns) || is_formula(fns)) {
    names <- names %||% "{str}"
    fns <- list(`1` = fns)
  } else {
    names <- names %||% "{str}_{fn}"
  }

  if (!is.list(fns)) {
    abort(c("Problem with `over()` input `.fns`.",
            i = "Input `.fns` must be a function or a list of functions"))
  }

  fns <- map(fns, rlang::as_function)

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




over <- function(.strs, .fns = NULL, ..., .names = NULL){

  setup <- over_setup({
    {
      .strs
    }
  }, fns = .fns, names = .names)
  vars <- setup$vars
  if (length(vars) == 0L) {
    return(tibble::new_tibble(list(), nrow = 1L))
  }
  fns <- setup$fns
  names <- setup$names

  data <-  across()

  n_strs <- length(vars)
  n_fns <- length(fns)
  seq_n_strs <- seq_len(n_strs)
  seq_fns <- seq_len(n_fns)
  k <- 1L
  out <- vector("list", n_strs * n_fns)

  for (i in seq_n_strs) {
    var <- vars[[i]]
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
