`.` <- function(x) {
  rlang::eval_tidy(rlang::sym(glue::glue(x,
                                         .open = "{",
                                         .close = "}",
                                         .envir = parent.frame())),
                   env = rlang::caller_env())
}


is.date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}


check_keep <- function() {

  call_st <- sys.calls()

  lapply(call_st, function(x) {

    .x <- x[[1]]

    if (any(grepl("^mutate", .x, perl = TRUE))) {

      keep_arg <- grepl("^\\.keep$", names(as.list(x)), perl = TRUE)

      if (any(keep_arg)) {
        keep_val <- as.list(x)[keep_arg]

        if (!keep_val %in% c("all", "none")) {
          rlang::abort(c("Problem with `over()` inside `mutate()`.",
                         i = '`over()` is not supported in `mutate()` calls which set the `.keep` argument to "used" or "unused".',
                         x = paste0('`over()` was called inside `mutate(..., .keep = "', keep_val ,'").'),
                         i = 'Either drop the `.keep` argument or set it to "all" (default) or "none".'))
        }
      }
    }

  })

}
