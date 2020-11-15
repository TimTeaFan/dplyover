# adapted from https://stackoverflow.com/a/60447909/9349302
is.date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

# function to check the `.keep` argument in the preceeding `mutate()` call
# some help from https://stackoverflow.com/questions/62746607/
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


