#' Selection helpers
#'
#' @description
#'
#' `dplyover` provides three kinds of selection helpers which are intended for
#' use in all functions that accept a vector as argument (that is `over()` and
#' `crossover()` as well as their variants, see here for a full list of the
#' [over-across function family][over_across_family]).
#'
#' Helpers which select **string parts** of the **column names** (of the underyling data):
#'  - [cut_names()] removes a specified pattern.
#'  - [extract_names()] extracts a specified pattern.
#'
#' Helpers which select **values** of a variable:
#'  - [dist_values()] returns all distinct values.
#'  - [seq_range()] returns the sequence between the `range()` of a variable.
#'
#' A helper function that evaluates a glue specification as variable
#'  - [.()] evaluates an interpolated string as symbol
#'
#' @name selection_helpers
#' @aliases over_selection_helpers
NULL
