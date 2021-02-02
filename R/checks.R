# adapted from https://stackoverflow.com/a/60447909/9349302
is.date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}


inspect_call <- function() {

  out <- list(warn = FALSE)
  trace_bck <- rlang::trace_back()
  call_fns <- lapply(trace_bck$calls, function(x) { `[[`(x, 1) })
  limit <- min(which(grepl("^dplyover::", call_fns)))
  mut_id <- which(grepl("^dplyr:::mutate", call_fns[1:limit - 1]))

  if (length(mut_id) > 0) {

    # calling_fn <- sub(".*::(.*)", "\\1", "dplyover::over")
    last_mut <- as.list(trace_bck$calls[[max(mut_id)-2]])

    keep_arg <- grepl("^\\.keep$", names(last_mut), perl = TRUE)

    if (any(keep_arg)) {
      keep_val <- last_mut[keep_arg]

      if (keep_val %in% c("used", "unused")) {
        out$warn <- TRUE
      }
    }
  }
  out
}
