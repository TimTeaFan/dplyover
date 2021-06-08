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

# environment where setup of dplyover calls is stored
setup_env <- rlang::new_environment()

# environment where last value of across2 pre/suf error is stored
.last <- rlang::new_environment()

# meta setup used by all major dplyover functions
meta_setup <- function(dep_call, setup_fn, ...) {

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

    # prepare dots to pass to do.call
    dots <- rlang::list2(...)

    # get last dplyr call
    last_verb_env <- last_verb()

    # get number of groups in original data
    n_grp <- dplyr::n_groups(get(".data", envir = sys.frame(last_verb_env)))
    # n_grp <- dplyr::n_groups(dynGet(".data"))

    # check keep for all functions except over
    if (!grepl("^over", dep_call, perl = TRUE)) {

      #> get keep arg
      keep_arg <- dynGet("keep", ifnotfound = NULL)

      #> display warning
      if (!is.null(keep_arg) && keep_arg %in% c("used", "unused")){
        call_nm <- sub("([A-z0-9_.]+).*", "\\1()", dep_call)
        rlang::warn(glue::glue("`{call_nm}` does not support the `.keep` argument in `dplyr::mutate()` when set to 'used' or 'unused'."))
      }
    }

    # simple setup up unique new call (only runs once per new call)
    if (!setup_exists) {

      # wipe complete setup_env on.exit of overarching call:
      do.call("on.exit",
              list(quote(rm(list  = ls(dplyover:::setup_env),
                            envir = dplyover:::setup_env)),
                   add = TRUE),
                   envir = sys.frame(1L))

      # to be save: delete call on.exit of last dplyr call
      do.call("on.exit",
              list(bquote(rm(list = .(dep_call),
                            envir = dplyover:::setup_env)),
                   add = TRUE),
              envir = sys.frame(last_verb_env))

      # TODO: when refactoring data access add last_verb_env to setup list?
      setup_env[[dep_call]] <- init_setup <- list(simple = TRUE,
                                                  n_grp = n_grp,
                                                  grp_no = grp_id,
                                                  setup = list(do.call(setup_fn, dots)))

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
             setup = append(init_setup_old$setup, list(do.call(setup_fn, dots))))

      return(init_setup$setup[[length(init_setup$grp_no)]])

    } # close complex setup else
  } # close setup else
}
