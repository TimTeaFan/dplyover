#' The over-across function family
#'
#' @description
#'
#' `dplyover` extends `dplyr`'s functionality by building a function family
#' around `dplyr::across()`.
#'
#' The goal of this **over-across function family** is to provide a concise and
#' uniform syntax which can be used to create columns by applying functions to
#' vectors and / or sets of columns in dplyr. Ideally, this will improve our
#' mental model so that it is easier to tackle problems where the solution is
#' based on creating new columns.
#'
#' The functions in the over-apply function family create columns by applying
#' one or several functions to:
#'
#' ### basic functions
#'  - [dplyr::across()]: a set of columns
#'  - [over()]: a vector (list or atomic vector)
#'
#' ### variants
#'  - [over2()] two vectors of the same length (pairwise)
#'  - [over2x()] two vectors (nested)
#'  - [across2()] two sets of columns (pairwise)
#'  - [across2x()] two sets of columns (nested)
#'  - [crossover()] a set of columns and a vector (pairwise)
#'
#' @name over_across_family
NULL
