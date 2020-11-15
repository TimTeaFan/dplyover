#' Evaluate an interpolated string as symbol
#'
#' @description
#'
#' ...
#'
#'
#' @param x ...
#'
#' @return
#' ...
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(c("Sepal", "Petal"),
#'               ~ .("{.x}.Width") + .("{.x}.Length")
#'               ))
#' ```
#'
#' The above syntax is equal to the more verbose:
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   mutate(over(c("Sepal", "Petal"),
#'               ~ eval(sym(paste0(.x, ".Width"))) +
#'                 eval(sym(paste0(.x, ".Length")))
#'               ))
#' ```
#'
#' @export
`.` <- function(x) {
  rlang::eval_tidy(rlang::sym(glue::glue(x,
                                         .open = "{",
                                         .close = "}",
                                         .envir = parent.frame())),
                   env = rlang::caller_env())
}
