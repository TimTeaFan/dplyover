test_prefix <- function(.data, .xcols, .ycols) {

  .xcols <- rlang::enexpr(.xcols)
  .ycols <- rlang::enexpr(.ycols)

  test_affix(data = .data,
             xcols = .xcols,
             ycols = .ycols,
             type = "prefix")
}

test_suffix <- function(.data, .xcols, .ycols) {

  .xcols <- rlang::enexpr(.xcols)
  .ycols <- rlang::enexpr(.ycols)

  test_affix(data = .data,
             xcols = .xcols,
             ycols = .ycols,
             type = "suffix")
}

test_affix <- function(data, xcols, ycols, type = c("prefix", "suffix")) {

  group_vars <- group_vars(data)

  if (length(group_vars) > 0) {
    data <- dplyr::ungroup(data)
    data <- dplyr::select(data, -dplyr::all_of(group_vars))
  }

  xvars <- tidyselect::eval_select(xcols, data)
  yvars <- tidyselect::eval_select(ycols, data)

  xvars <- names(xvars)
  yvars <- names(yvars)

  if (length(xvars) != length(yvars)) {
    rlang::abort(c(paste0("Problem with `test_", type,"()` input `.xcols` and `.ycols`."),
                   i = "Input `.xcols` and `.ycols` must use the same number of columns.",
                   x = paste0(length(xvars), " columns are selected in `.xcols`, ",
                              "while ", length(yvars), " columns are selected in `.colsy`.")))
  }

  var_nms <- purrr::flatten(purrr::map2(xvars, yvars, ~ list(c(.x, .y))))
  if (type == "prefix") {
    res <- purrr::map(var_nms, ~ get_affix(.x, "prefix"))
  } else {
    res <- purrr::map(var_nms, ~ get_affix(.x, "suffix"))
  }

  res <- unlist(purrr::modify_if(res, rlang::is_empty, ~ NA_character_))

  inp_tbl <- tibble::tibble(.xcols = xvars,
                            .ycols = yvars,
                            !! type := res)

  print(inp_tbl, n = 10)
  if (nrow(inp_tbl) > 10) {
    cat("Use `.Last.value %>% View()` to see to full list of variables.")
    invisible(inp_tbl)
  }
}

# helper function for across2_setup
get_affix <- function(x, type = c("prefix", "suffix")) {

  side <- switch(type,
                 "prefix" = "right",
                 "suffix" = "left")

  x <- stringr::str_pad(x, max(nchar(x)), side = side, pad = " ")
  x_ls <- purrr::transpose(strsplit(x, ""))
  x_ls_length <- purrr::map_dbl(purrr::map(x_ls, unique), length)
  x_rle <- rle(x_ls_length)
  if (side == "right" && x_rle$values[1] == 1) {
    res <- stringr::str_sub(x[[1]],
                            start = 1L,
                            end = x_rle$length[1])

  } else if (side == "left" && x_rle$values[length(x_rle$values)] == 1) {
    res_start <- sum(x_rle$length[-length(x_rle$length)]) + 1
    res_length <- x_rle$length[length(x_rle$length)]
    res_end <- res_start + res_length

    res <- stringr::str_sub(x[[1]],
                            start = res_start,
                            end = res_end)
  } else {
    res <- NULL
  }

  res <- stringr::str_remove_all(res, "[:punct:]*$")
  res <- stringr::str_remove_all(res, "^[:punct:]*")

  if (side == "right") {
    res <- stringr::str_extract(res, "^[:alnum:]*")
  } else {
    res <- stringr::str_extract(res, "[:alnum:]*$")
  }

  res

}

# add to tests
# x <- c("Sepal.Length", "Sepal.Width")
# x <- c("Length.Sepal", "Width.Sepal")
# x <- c("Length.of.Sepal.here", "Length.no.Sepal.here")
# get_affix(x, "suffix")
