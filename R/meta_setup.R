meta_setup <- function(grp_id, dep_call, par_frame, setup_fn, ...) { # data = NULL, xcols = NULL, ycols = NULL

  call_nm <- sub("([a-z0-9]+).*", "\\1()", dep_call)

  dots <- rlang::list2(...)

  wrong_setup <- FALSE

  # meta setup
  setup_exists <- exists(".__dplyover_setup__.", envir = par_frame)

  # if setup already exists
  if (setup_exists && grp_id > 1L) {
    # get data
    parent_setup <- get(".__dplyover_setup__.", envir = par_frame)
    # get call number
    call_no <- which.min(parent_setup$call_his)
    call_id <- paste0("call", call_no)
    # update "call_his"
    par_frame[[".__dplyover_setup__."]][["call_his"]][call_no] <- grp_id
    # check call and get data from existing call
    if (identical(parent_setup$call_lang[call_no], dep_call)) {
      return(parent_setup[[call_id]]$setup)
    }
    # otherwise continue
    wrong_setup <- TRUE
  }
  # if this is a new call to over or if setup went wrong
  if (!setup_exists || wrong_setup) {
      # new setup
      if (grp_id == 1) {
        call_info <- inspect_call()
        if (call_info[["warn"]])
        rlang::warn(glue::glue("`{call_nm}` does not support the `.keep` argument in `dplyr::mutate()` when set to 'used' or 'unused'."))
        }
      par_frame[[".__dplyover_setup__."]][["call_his"]] <- grp_id
      par_frame[[".__dplyover_setup__."]][["call_lang"]] <- dep_call
      call_id <- paste0("call", grp_id)
  # existing setup, but new call
  } else {
      parent_setup <- get(".__dplyover_setup__.", envir = par_frame)
      # register new call
      par_frame[[".__dplyover_setup__."]][["call_his"]] <- c(parent_setup$call_his, 1)
      par_frame[[".__dplyover_setup__."]][["call_lang"]] <- c(parent_setup$call_lang, dep_call)
      # get number of current call
      call_id <- paste0("call", which.min(parent_setup$call_his))
  }

  # in both cases: write data into par_frame
  par_frame[[".__dplyover_setup__."]][[call_id]][["setup"]] <-
  setup <- do.call(setup_fn, dots)

  setup
}
