#' Evaluate an interpolated string as symbol
#'
#' @description
#'
#' This function takes a glue specifcation as input, and evaluates the final
#' argument string as name in the caller environment.
#'
#' @param x A glue specification, that is, a string which contains an R expression
#'   wrapped in curly braces, e.g. `."{.x}_some_string"`.
#'
#' @return
#' The values of the variable with the name of the final argument string, given
#' that it exists in the caller environment.
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' ```{r, comment = "#>", collapse = TRUE}
#' library(dplyr)
#'
#' # For better printing
#' iris <- as_tibble(iris)
#' ```
#'
#' Below is a simple example from `over()`. In  `over`'s function
#' argument `.x` is first evaluated as 'Sepal' and then as 'Petal' which
#' results in the final argument strings 'Sepal.Width' and 'Sepal.Length' as
#' well as 'Petal.Width' and 'Petal.Length'.
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
#' Although `.()` was created with the use of `over()` in mind, it can also be
#' used within `dplyr::across()` in combination with `dplyr::cur_column()`.
#' First let's rename 'Sepal.Length' and 'Petal.Length' to 'Sepal' and 'Petal'
#' to have a stem to which we can attach the string '.Width' to access the
#' two 'Width' variables. Now we can call `.(cur_colunm())` to access the variable
#' `across()` has been called on (Note: we could have used `.x` instead). We can
#' further access the values of the 'Width' variables by wrapping `cur_column()`
#' in curly braces `{}`, adding  `.Width` and wrapping everything with
#' quotation marks `.("{cur_column()}.Width")`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   rename("Sepal" = "Sepal.Length",
#'          "Petal" = "Petal.Length") %>%
#'    mutate(across(c(Sepal, Petal),
#'                  ~ .(cur_column()) + .("{cur_column()}.Width"),
#'                  .names = "{col}_sum"))
#' ```
#'
#' A similar approach can be achieved using `purrr::map` in combination with `.()`:
#' ```{r, comment = "#>", collapse = TRUE}
#' iris %>%
#'   rename("Sepal" = "Sepal.Length",
#'          "Petal" = "Petal.Length") %>%
#'   mutate(purrr::map_dfc(c("Sepal_sum" = "Sepal", "Petal_sum" = "Petal"),
#'                         ~ .(.x) + .("{.x}.Width")))
#' ```
#' @export
`.` <- function(x) {
  rlang::eval_tidy(rlang::sym(glue::glue(x,
                                         .open = "{",
                                         .close = "}",
                                         .envir = parent.frame())),
                   env = rlang::caller_env())
}
