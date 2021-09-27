# Internal metasetup for over-across family

# environment where setup of dplyover calls is stored
setup_env <- rlang::new_environment()

# environment where last value of across2 pre/suf error is stored
# FIXME: move to different script
last_err <- rlang::new_environment()
#
# # environment where last value of across2 pre/suf error is stored
# # FIXME: move to different script
# .current_col <- rlang::new_environment()
#
# across_env <- rlang::new_environment()

# deparse call (similar to dplyr:::key_deparse)
# this function is copied from dplyr
# see README section Acknowledgements as well as dplyr's license and copyright
deparse_call <- function(call) {
  deparse(call,
          width.cutoff = 500L,
          backtick = TRUE,
          nlines = 1L,
          control = NULL)
}


# meta setup used by all major dplyover functions
meta_setup <- function(dep_call, setup_fn, ..., dplyr_env = NULL) {

  # check if setup for this call exists
  setup_exists <- exists(dep_call, envir = setup_env)

  # get group id
  #> if setup already exists directly
  if (setup_exists) {
    grp_id <- dplyr::cur_group_id()

  #> otherwise using tryCatch
  } else {
    grp_id <- tryCatch({
      dplyr::cur_group_id()
    }, error = function(e) {
      call_nm <- sub("([A-z0-9_.]+).*", "\\1()", dep_call)
      rlang::abort(glue::glue("`{call_nm}` must only be used inside dplyr verbs."))
    })

  }

  # if setup already exists and group id > 1 (runs for each group)
  if (setup_exists && grp_id > 1L) {

    # get data
    init_setup <- get(dep_call, envir = setup_env)

    # if simple setup
    if (init_setup$simple) {
      act_call_no <- 1

    # if complex setup
    } else {
      # get active call number
      act_call_no <- which.min(init_setup$grp_no)
      # update group number to Inf if last group
      if (init_setup$n_grp[act_call_no] == grp_id) {
        setup_env[[dep_call]]$grp_no[act_call_no] <- Inf
      } else {
        setup_env[[dep_call]]$grp_no[act_call_no] <- grp_id
      }
    }

    return(init_setup$setup[[act_call_no]])

  # setup
  } else {

    # get last dplyr call
    last_dplyr_frame <- last_verb()

    # prepare dots to pass to do.call
    dots <- rlang::list2(...)

    # get number of groups in original data
    # FIXME: don't create copy of data, use reference instead
    dat <- get_init_data(last_dplyr_frame)
    n_grp <- dplyr::n_groups(dat)

    # checks for all functions except over
    if (!grepl("^over", dep_call, perl = TRUE)) {

      call_nm <- sub("([A-z0-9_.]+).*", "\\1()", dep_call)

      # check keep
      #> get keep arg
      keep_arg <- dynGet("keep", ifnotfound = NULL)

      #> display warning
      if (!is.null(keep_arg) && keep_arg %in% c("used", "unused")){
        rlang::warn(glue::glue("`{call_nm}` does not support the `.keep` argument in `dplyr::mutate()` when set to 'used' or 'unused'."))
      }

    # check dplyr env

    col_names <- sort(names(dplyr::cur_data_all()))
    dplyr_env <- -1

    # FIXME: put this logical in a function which can be improved gradually:
    for (e in seq_along(sys.parents())) {
      if (identical(sort(names(rlang::env_parent(parent.frame(e), n = 1))), col_names)) {
        dplyr_env <- e-1
        break
      }
    }
    if (dplyr_env == -1) {
      #FIXME: Not much needed instead try looping over other nested enviroments
      if (identical(sort(names(get_fns_env(call_nm, dots$fns))),
                   col_names)) {
        dplyr_env <- 0
      } else {
        rlang::warn("Bad performance due to improper environment handling.")
      }
    }
    }


    # simple setup up unique new call (only runs once per new call)
    if (!setup_exists) {

      # wipe complete setup_env on.exit of overarching dplyr call:
      do.call("on.exit",
              list(quote(suppressWarnings(
                          rm(list  = ls(setup_env),
                             envir = setup_env))
                         ),
                   add = TRUE),
              envir = sys.frame(last_dplyr_frame))

      setup_env[[dep_call]] <- init_setup <- list(simple = TRUE,
                                                  n_grp = n_grp,
                                                  grp_no = grp_id,
                                                  setup = list(append(do.call(setup_fn, dots),
                                                               list(dplyr_env = dplyr_env))))

      return(init_setup$setup[[1]])

  # complex setup of new duplicate call (only runs once per new call)
    } else {

      # get existing call
      init_setup_old <- get(dep_call, envir = setup_env)

      # if existing call was simple and if call is completed: set grp_id to Inf
      if (init_setup_old$simple && init_setup_old$n_grp == init_setup_old$grp_no) {
        init_setup_old$grp_no <- Inf
      }

      setup_env[[dep_call]] <- init_setup <-
        list(simple = FALSE,
             n_grp = c(init_setup_old$n_grp, n_grp),
             grp_no = c(init_setup_old$grp_no, grp_id),
             setup = append(init_setup_old$setup,
                            list(append(do.call(setup_fn, dots),
                                        list(dplyr_env = dplyr_env)))
                            )
             )

      return(init_setup$setup[[length(init_setup$grp_no)]])

    } # close complex setup else
  } # close setup else
}
