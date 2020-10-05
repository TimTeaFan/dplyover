dist_values <- function(.var, .sort = c("asc", "desc", "none"), .dat = NULL) {

  if (is.null(.dat) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .dat <- dplyr::across()
  }

  sort <- match.arg(.sort)

  var <- rlang::as_string(rlang::ensym(.var))

  if (is.factor(.dat[[var]])) {
    res <- levels(.dat[[var]])
  } else {
    res <- unique(.dat[[var]])
  }

  if (sort == "asc") {
    sort(res)
  } else if (sort == "desc") {
    sort(res, decreasing = TRUE)
  } else {
    res
  }

}




