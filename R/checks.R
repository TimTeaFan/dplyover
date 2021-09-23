# adapted from https://stackoverflow.com/a/60447909/9349302
is.date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

inspect_call <- function(warn = TRUE, last_verb = FALSE) {

  out <- list(warn = FALSE,
              last_verb = NULL)
  trace_bck <- rlang::trace_back()

  if (is.null(trace_bck$calls)) {
    call_fns <- purrr::map(purrr::transpose(trace_bck), function(trace) {
                  paste0(trace$namespace,
                    trace$scope,
                    as.character(trace$call[1]))})
  } else {
    call_fns <- purrr::map(trace_bck$calls, function(call)  `[[`(call, 1) )
  }
  limit <- min(which(grepl("^dplyover::", call_fns)))
  mut_id <- which(grepl("^dplyr:::mutate", call_fns[1:limit - 1]))

  # last dplyr verb
  if (last_verb) {
  last_dplyr_verb <- max(which(grepl("^dplyr:::[mutate|summarise|summarize|filter|select|arrange|transmute]", call_fns)))
  out$last_verb <- last_dplyr_verb
  }
  # check keep
  if (warn) {
      if (length(mut_id) > 0) {

      last_mut <- as.list(trace_bck$call[[max(mut_id) - 2]])

      keep_arg <- grepl("^\\.keep$", names(last_mut), perl = TRUE)

      if (any(keep_arg)) {
        keep_val <- last_mut[keep_arg]

        if (keep_val %in% c("used", "unused")) {
          out$warn <- TRUE
        }
      }
    }
  }
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

filter
