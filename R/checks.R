# adapted from https://stackoverflow.com/a/60447909/9349302
is.date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

# function to check the `.keep` argument in the preceeding `mutate()` call
# some help from https://stackoverflow.com/questions/62746607/
check_keep <- function(type = c("keep", "summarise")) {

  call_st <- sys.calls()

  calling_fn <- call_st[[sys.nframe() - 1]][[1]]

  res <- lapply(call_st, function(x) {

    .x <- x[[1]]

    if ("keep" %in% type) {

    if (any(grepl("^mutate", .x, perl = TRUE))) {

      keep_arg <- grepl("^\\.keep$", names(as.list(x)), perl = TRUE)

      if (any(keep_arg)) {
        keep_val <- as.list(x)[keep_arg]

        if (!keep_val %in% c("all", "none")) {
          rlang::abort(c(paste0("Problem with `", calling_fn,"()` inside `mutate()`."),
                         i = paste0('`', calling_fn, '`() is not supported in `mutate()` calls which set the `.keep` argument to "used" or "unused".'),
                         x = paste0('`', calling_fn, '`() was called inside `mutate(..., .keep = "', keep_val ,'").'),
                         i = 'Either drop the `.keep` argument or set it to "all" (default) or "none".'))
        }
      }
    }
    }

    if ("summarise" %in% type) {

      v1 <- tryCatch({
        rlang::env_name(environment(fun = eval(.x)))
      }, error = function(e) {
        NA
      })

      v2 <- any(grepl("^summari[s|z]e", .x, perl = TRUE))

      v2 & v1 == "namespace:dplyr"
    }

  })

  if ("summarise" %in% type && !any(unlist(res))) {
    rlang::abort(c(paste0("Problem with `", calling_fn,"()`."),
                   x = paste0('`', calling_fn, '`() must only be used inside `dplyr::summarise()`')))

  }

}


