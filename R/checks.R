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

    last_mut <- as.list(trace_bck$calls[[max(mut_id) - 2]])

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

# this is the only internal dplyr function internalized in dplyover
# see README section Acknowledgements as well as dplyr's license and copyright
data_mask_top <- function(env, recursive = FALSE, inherit = FALSE) {
  while (rlang::env_has(env, ".__tidyeval_data_mask__.", inherit = inherit)) {
    env <- rlang::env_parent(rlang::env_get(env, ".top_env", inherit = inherit))
    if (!recursive) {
      return(env)
    }
  }

  env
}
