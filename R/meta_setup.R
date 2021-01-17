meta_setup <- function(grp_id, dep_call, par_frame, setup_fn, warn_keep = TRUE, safe_names = FALSE, ..., data = NULL, xcols = NULL, ycols = NULL) {

  call_nm <- sub("([a-z0-9]+).*", "\\1()", dep_call)

  dots <- rlang::list2(...)

  if (!is.null(xcols)) {
    dots[["xcols"]] <- xcols
  }

  if (!is.null(ycols)) {
    dots[["ycols"]] <- ycols
  }

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
    # check call
    if (!identical(parent_setup$call_lang[call_no], dep_call)) {
      rlang::abort(c(paste0("Problem with the way `,", call_nm, "` was called."),
                     i = paste0("It is likely that the call to `", call_nm, "` was not made from a top level dplyr verb."),
                     x = paste0("For technical reasons, it is not possible that the call to `", call_nm,"` depends on the group_id.")))
    }
    # get data from existing call
    return(parent_setup[[call_id]]$setup)

    # if this is a new call to over: update par_frame
  } else {
    if (!setup_exists) {
      # new setup
      if (warn_keep) {
        check_keep(type = "keep")
        }
      par_frame[[".__dplyover_setup__."]][["call_his"]] <- grp_id
      par_frame[[".__dplyover_setup__."]][["call_lang"]] <- dep_call
      call_id <- paste0("call", 1)

      # existing setup, but new call
    } else {
      parent_setup <- get(".__dplyover_setup__.", envir = par_frame)
      # register new call
      par_frame[[".__dplyover_setup__."]][["call_his"]] <- c(parent_setup$call_his, 1)
      par_frame[[".__dplyover_setup__."]][["call_lang"]] <- c(parent_setup$call_lang, dep_call)
      # get number of current call
      call_id <- paste0("call", which.min(parent_setup$call_his))
    }

    # write data into parent frame
    if (is.null(data) && safe_names) {
      data <- dplyr::cur_data()[1, ]
    } else {
    dots[["data"]] <- data
    }

    par_frame[[".__dplyover_setup__."]][[call_id]][["setup"]] <-
    setup <- do.call(setup_fn, dots)

    if (safe_names) {

      .cnames <- names(data)

      if (safe_names && any(setup$names %in% .cnames)) {
        dnames <- .cnames[.cnames %in% setup$names]
        names_l <- ifelse(length(dnames) > 3, 3, length(dnames))

        rlang::abort(c("Problem with `over()`.",
                       i = "Output must not contain existing column names.",
                       x = paste0(paste0("`", call_nm, "` tried to create the following existing column names: "),
                                  paste0(paste0("'", dnames[seq_along(1:names_l)], "'"), collapse = ", "),
                                  ifelse(length(dnames) > 3, " etc. ", ".")),
                       i = "If you want to transform existing columns try using `dplyr::across()`.",
                       i = "If you want to change the output names use the `.names` argument."))

      }
    }

  }
  setup
}
