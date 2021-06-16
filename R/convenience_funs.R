#' Compare a vector against a value for equality of values
#'
#' @description
#' ...
#'
#' @param check An atomic vector.
#' @param against A value of length one against the values in `check` are compared.
#'
#' @return
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' Show all special cases:
#' check is factor with NA
#' against is named
#' 1 == 1L
#'
#' @export
same_value <- function(check, against) {

  if (length(against) > 1) {
    rlang::abort(c("Problem with `same_value()` input `against`.",
                   i = "Only vectors of length 1 allowed in `against`.",
                   x = glue::glue("Length of y is {length(against)}.")))
  }

  if (is.na(against)) {

    return(is.na(check))

  } else {

    return(ifelse(is.na(check), FALSE, check == against))

  }
}
