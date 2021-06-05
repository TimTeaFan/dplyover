# deprase call (similar to dplyr:::key_deparse)
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

## meta setup use by all major dplyover functions (tests passing)
#> this setup is rather dodgy and currently being overhauled
#> see new_meta_setup branch!
#> and yes, we shouldn't write something in par_frame since dplyover does not create this environment
meta_setup <- function(grp_id, dep_call, setup_fn, ...) {

  dots <- rlang::list2(...)

  # meta setup
  setup_exists <- exists(dep_call, envir = setup_env)

  # if setup already exists and group id > 1
  if (setup_exists && grp_id >= 1L) {

    # get data
    init_setup <- get(dep_call, envir = setup_env)

    # if simple setup
    if (!init_setup$complex) {
      # TODO: specify which part of parent_setup

      #> wipe dep_call setup if n_grp is grp_id
      if (grp_id == init_setup$n_grp) {
        on.exit(rm(dep_call, envir = setup_env))
      }

      return(init_setup$setup)

    # if complex setup
    } else {
      # TODO: specify

      # add steps complex setup

      # # get call number
      # call_no <- which.min(parent_setup$call_his)
      # call_id <- paste0("call", call_no)
      # # update "call_his"
      # par_frame[[".__dplyover_setup__."]][["call_his"]][call_no] <- grp_id
      # # check call and get data from existing call
      # if (identical(parent_setup$call_lang[call_no], dep_call)) {
      #   return(parent_setup[[call_id]]$setup)
      # }

      return(init_setup$setup)
    }
  # setup new unique call
  } else if (!setup_exists) {

    # TODO: delete after tests:
    if(grp_id > 1L) stop("Ups. Setup went wrong.")


    ## new setup ##

    #> get number of groups in original data
    n_grp <- dplyr::n_groups(dynGet(".data"))

    #> get call name
    call_nm <- sub("([A-z0-9_.]+).*", "\\1()", dep_call)

    #> check keep for all functions except over
    if (!grepl("^over", call_nm, perl = TRUE)) {

        # check keep:
        keep_arg <- dynGet("keep", ifnotfound = NULL)

        if (!is.null(keep_arg) && keep_arg %in% c("used", "unused")){
          rlang::warn(glue::glue("`{call_nm}` does not support the `.keep` argument in `dplyr::mutate()` when set to 'used' or 'unused'."))
        }
    }

    #> wipe dep_call setup if n_grp is grp_id (here special case for n_grp = 1)
    if (grp_id == n_grp) {
      on.exit(rm(list = dep_call, envir = setup_env), add = TRUE)
    }

    #> wipe complete setup_env on.exit of overarching dplyr call:
    #>> check the frame in which the last dplyr verb is used
    last_dplyr_env <- sys.frame(last_verb())

    # >> add on.exit to that environment
    do.call("on.exit",
            list(quote(rm(list  = ls(setup_env),
                          envir = setup_env)),
                 add = TRUE),
                 envir = last_dplyr_env)

    setup_env[[dep_call]] <- init_setup <- list(complex = FALSE,
                                                n_grp = n_grp,
                                                setup = do.call(setup_fn, dots))

    return(init_setup$setup)
  # setup new duplicate call
  } else {
  # TODO:
  }
}
