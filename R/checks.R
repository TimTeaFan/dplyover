# adapted from https://stackoverflow.com/a/60447909/9349302
is.date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

last_verb <- function() {

  # get trace_back info
  trace_bck <- rlang::trace_back()

  # get call names
  call_fns <- lapply(trace_bck$calls, function(x) { `[[`(x, 1) })

  # get last dplyr verb
  out <- max(which(grepl("^dplyr:::[mutate|summarise|summarize|filter|select|arrange|transmute]", call_fns)))

  out
}

# this function is copied from dplyr
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
