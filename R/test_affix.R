test_prefix <- function(.data, .cols1, .cols2) {
  test_affix(data = .data,
             cols1 = .cols1,
             cols2 = .cols2,
             type = "prefix")
}

test_suffix <- function(.data, .cols1, .cols2) {
  test_affix(data = .data,
             cols1 = .cols1,
             cols2 = .cols2,
             type = "suffix")
}

# test_affix()
test_affix <- function(data, cols1, cols2, type = c("prefix", "suffix")) {

  group_vars <- group_vars(data)

  if (length(group_vars) > 0) {
    data <- dplyr::ungroup(data)
    data <- dplyr::select(data, -dplyr::all_of(group_vars))
  }

  cols1 <- rlang::enquo(cols1)
  cols2 <- rlang::enquo(cols2)

  vars1 <- tidyselect::eval_select(rlang::expr(!!cols1), data)
  vars2 <- tidyselect::eval_select(rlang::expr(!!cols2), data)

  vars1 <- names(vars1)
  vars2 <- names(vars2)

  if (length(vars1) != length(vars2)) {
    rlang::abort(c(paste0("Problem with `test_", type,"()` input `.cols1` and `.cols2`."),
                   i = "Input `.cols1` and `.cols2` must use the same number of columns.",
                   x = paste0(length(vars1), " columns are selected in `.cols1`, ",
                              "while ", length(vars2), " columns are selected in `.cols2`.")))
  }

  var_nms <- purrr::flatten(purrr::map2(vars1, vars2, ~ list(c(.x, .y))))
  if (type == "prefix") {
    res <- purrr::map_chr(var_nms, ~ get_affix(.x, "prefix"))
  } else {
    res <- purrr::map_chr(var_nms, ~ get_affix(.x, "suffix"))
  }

  inp_tbl <- tibble::tibble(.cols1 = vars1,
                            .cols2 = vars2,
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
  if (side == "right") {
    res <- stringr::str_sub(x[[1]],
                            start = 1L,
                            end = x_rle$length[1])

  } else {
    res_start <- sum(x_rle$length[-length(x_rle$length)])
    res_length <- x_rle$length[length(x_rle$length)]
    res_end <- res_start + res_length

    res <- stringr::str_sub(x[[1]],
                            start = res_start,
                            end = res_end)
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
# get_affix(x, "prefix")
