# helper functions which help dealing with dplyr internals ...
# ... without relying on dplyr's internal context functions and data mask
#

cur_x_column <- function() {
  if (length(setup_env$xcol) == 0) {
    stop("`cur_x_column()` not available in this context.")
  }
  setup_env$xcol
}

cur_y_column <- function() {
  if (length(setup_env$ycol) == 0) {
    stop("`cur_y_column()` not available in this context.")
  }
  setup_env$ycol
}

last_verb <- function() {

  # get trace_back info
  trace_bck <- rlang::trace_back()

  # get call names depending on rlang version
  if (is.null(trace_bck$calls)) { # rlang version 0.4.11.9001
    call_fns <- purrr::map(purrr::transpose(trace_bck), function(trace) {
      paste0(trace$namespace,
             trace$scope,
             as.character(trace$call[1]))})
  } else { # earlier rlang version
    call_fns <- purrr::map(trace_bck$calls, function(call)  `[[`(call, 1) )
  }

  # get last dplyr verb
  out <- max(which(grepl("^dplyr:::[mutate|summarise|summarize|filter|select|arrange|transmute]", call_fns)))

  call_nm <- rlang::call_name(trace_bck[["call", exact = FALSE]][[out]])

  # verify that trace_back() covers all calls
  sc <- lapply(sys.calls(), function(x) `[[`(x, 1))

  if(length(sc) > length(trace_bck[["call", exact = FALSE]])) {
    for (i in rev(seq_along(sc))) {
      if (as.character(sc[[i]])[1] == call_nm) {
        out <- i
        break
      }
    }
  }

  names(out) <- gsub("_.*", "", call_nm)
  out
}

get_init_data <- function(dplyr_frame_no) {

  dat <- get(".data", envir = sys.frame(dplyr_frame_no))

  # if (is.null(dat) || inherits(dat, "rlang_fake_data_pronoun")) {
  #   dat <- dynGet(".data", ifnotfound = NULL)
  # }

  if (is.null(dat) || inherits(dat, "rlang_fake_data_pronoun")) {
    rlang::abort(c("Problem with `get_init_data()`.",
                   x = "Could not access underlying dplyr data.",
                   i = "Please report your call and session info under:\nhttps://github.com/TimTeaFan/dplyover/issues/14"))
  }

  dat

}

fget_init_data <- function() {

  dat <- dynGet(".data")

  if (is.null(dat) || inherits(dat, "rlang_fake_data_pronoun")) {
    dat <- dynGet(".data", minframe = 3L, ifnotfound = NULL)
  }

  if (is.null(dat) || inherits(dat, "rlang_fake_data_pronoun")) {
    rlang::abort(c("Problem with `get_init_data()`.",
                   x = "Could not access underlying dplyr data.",
                   i = "Please report your call and session info under:\nhttps://github.com/TimTeaFan/dplyover/issues/14"))
  }

  dat

}

rebuild_data <- function(dplyr_frame_no) {

  mutate_env <- sys.frame(dplyr_frame_no)

  if (mutate_env$i == 1L && length(mutate_env$new_columns) == 0) {
    return(get_init_data(dplyr_frame_no))

  } else if ((mutate_env$i) - 1L == length(mutate_env$new_columns)) {
    # setup: get initial and new data
    dat <- get_init_data(dplyr_frame_no)
    new_dat <- mutate_env$new_columns

    # differentiate between new and existing columns
    new_cols <- setdiff(names(new_dat), names(dat))
    replace_cols <- intersect(names(new_dat), names(dat))

    # replace existing columns
    dat[, replace_cols] <- new_dat[replace_cols]
    # cbind new columns
    dat <- cbind(dat, new_dat[new_cols])

    return(dat)

  } else {
    rlang::abort(rlang::abort(c("Problem with `rebuild_data()`.",
                                x = "Could not rebuild underlying dplyr data.",
                                i = "Please report your call and session info under:\nhttps://github.com/TimTeaFan/dplyover/issues/15")))
  }
}

get_full_data <- function(dplyr_frame_no) {

  switch(names(dplyr_frame_no),
         "transmute" = ,
         "mutate" = rebuild_data(dplyr_frame_no),
         get_init_data(dplyr_frame_no)
         )
}

access_groups <- function(dplyr_frame_no) {

  mutate_env <- sys.frame(dplyr_frame_no)

  n_grps <- length(mutate_env$rows)

  if(is.null(n_grps) || length(n_grps) == 0) {
    n_grps <- dplyr::n_groups(get_init_data(dplyr_frame_no))
  }

  n_grps
}

faccess_groups <- function() {

  n_grps <- length(dynGet("rows", ifnotfound = NULL))

  if(is.null(n_grps) || length(n_grps) == 0) {
    n_grps <- dplyr::n_groups(fget_init_data())
  }

  n_grps
}

get_n_groups <- function(dplyr_frame_no) {

  switch(names(dplyr_frame_no),
         "transmute" = ,
         "mutate" = access_groups(dplyr_frame_no),
         dplyr::n_groups(get_init_data(dplyr_frame_no))
  )
}

get_fns_env <- function(cl, fns) {

  if (rlang::is_formula(fns) || is.function(fns)) {
    env <- environment(fns)
    if(!is.null(env)) return(rlang::env_parent(env))

  } else if (is.list(fns)) {
    env <- environment(fns[[1]])
    if(!is.null(env)) return(rlang::env_parent(env))

  } else {
    return(NULL)
  }
}

# get_col <- function {
#
# }

# this function is one of the few copied functions from dplyr
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
