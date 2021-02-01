
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dplyover

<!-- badges: start -->

[![R-CMD-check](https://github.com/TimTeaFan/dplyover/workflows/R-CMD-check/badge.svg)](https://github.com/TimTeaFan/dplyover/actions)
[![Codecov test
coverage](https://codecov.io/gh/TimTeaFan/dplyover/branch/main/graph/badge.svg)](https://codecov.io/gh/TimTeaFan/dplyover?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/timteafan/dplyover/badge)](https://www.codefactor.io/repository/github/timteafan/dplyover)
<!-- badges: end -->

## Overview

{dplyover} extends {dplyr}’s functionality by building a function family
around `dplyr::across()`.

The goal of this *over-across function family* is to provide a concise
and uniform syntax which can be used to create columns by applying
functions to vectors and/or sets of columns in {dplyr}. Ideally, this
will:

  - *reduce the amount of code* to create variables derived from
    existing colums, which is especially helpful when doing explanatory
    data analysis (e.g. lagging, collapsing, recoding many variables in
    a similar way).
  - *provide a clean {dplyr} approach* to create many variables which
    are calculated based on two or more varialbes.
  - *improve our mental model* so that it is easier to tackle problems
    where the solution is based on creating new columns.

The functions in the *over-apply function family* create columns by
applying one or several functions to:

  - `dplyr::across()`: a set of columns (not part of dplyover)
  - `over()`: a vector (list or atomic vector)
  - `over2()` two vectors of the same length (pairwise)
  - `over2x()` two vectors (nested)
  - `across2()` two sets of columns (pairwise)
  - `across2x()` two sets of columns (nested)
  - `crossover()` a set of columns and a vector (pairwise)
  - `crossoverx()` a set of columns and a vector (nested)

## Installation

{dplyover} is not on CRAN. You can install the latest version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TimTeaFan/dplyover")
```

## Getting started

Below are a few examples of the <over-across function family>. More
functions and workarounds of how to tackle the problems below without
{dplyover} can be found in the vignette “Why dplyover?”.

``` r
# dplyover is an extention of dplyr on won't work without it
library(dplyr)
library(dplyover)

# For better printing:
iris <- as_tibble(iris)
```

#### Applying a function with a varying argument to one column

`over()` applies one or several functions to a vector. We can use it
inside `dplyr::mutate()` to create several similar variables that we
derive from an existing column. This is helpful in cases where we want
to create a batch of similar variables with only slightly changes in the
arguments of the calling function. A good example are `lag` and `lead`
variables. Below we use column ‘a’ to create lag and lead variables by
`1`, `2` and `3` positions. `over()`’s `.names` argument lets us put
nice names on the output columns.

``` r
tibble(a = 1:25) %>%
  mutate(over(c(1:3),
              list(lag  = ~ lag(a, .x),
                   lead = ~ lead(a, .x)),
              .names = "a_{fn}{x}"))
#> # A tibble: 25 x 7
#>       a a_lag1 a_lead1 a_lag2 a_lead2 a_lag3 a_lead3
#>   <int>  <int>   <int>  <int>   <int>  <int>   <int>
#> 1     1     NA       2     NA       3     NA       4
#> 2     2      1       3     NA       4     NA       5
#> 3     3      2       4      1       5     NA       6
#> 4     4      3       5      2       6      1       7
#> # ... with 21 more rows
```

#### Applying a function with a varying argument to a set of columns

`crossoverx()` applies the functions in `.fns` to every combination of
colums in `.xcols` with elements in `.y`. This is similar to the example
above, but this time, we use a set of columns. Below we create five
lagged variables for each ‘Sepal.Length’ and ‘Sepal.Width’. Again, we
use a named list as argument in `.fns` to create nice names by
specifying the glue syntax in `.names.`

``` r
iris %>%
   transmute(
     crossoverx(starts_with("sepal"),
                1:5,
                list(lag = ~ lag(.x, .y)),
                .names = "{xcol}_{fn}{y}")) %>%
   glimpse
#> Rows: 150
#> Columns: 10
#> $ Sepal.Length_lag1 <dbl> NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, ...
#> $ Sepal.Length_lag2 <dbl> NA, NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4...
#> $ Sepal.Length_lag3 <dbl> NA, NA, NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5....
#> $ Sepal.Length_lag4 <dbl> NA, NA, NA, NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6...
#> $ Sepal.Length_lag5 <dbl> NA, NA, NA, NA, NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4,...
#> $ Sepal.Width_lag1  <dbl> NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, ...
#> $ Sepal.Width_lag2  <dbl> NA, NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2...
#> $ Sepal.Width_lag3  <dbl> NA, NA, NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3....
#> $ Sepal.Width_lag4  <dbl> NA, NA, NA, NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4...
#> $ Sepal.Width_lag5  <dbl> NA, NA, NA, NA, NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9,...
```

#### Applying functions to a pair of variables

`across2()` can be used to transform pairs of variables in one or more
functions. In the example below we want to calculate the product and the
sum of all pairs of ‘Length’ and ‘Width’ variables in the `iris` data
set. We can use `{pre}` in the glue specification in `.names` to extract
the common prefix of each pair of variables. We can further transform
the names, in the example setting them `tolower`, by specifying the
`.names_fn` argument:

``` r
iris %>%
  transmute(across2(ends_with("Length"),
                    ends_with("Width"),
                    .fns = list(product = ~ .x * .y,
                                sum = ~ .x + .y),
                   .names = "{pre}_{fn}",
                   .names_fn = tolower))
#> # A tibble: 150 x 4
#>   sepal_product sepal_sum petal_product petal_sum
#>           <dbl>     <dbl>         <dbl>     <dbl>
#> 1          17.8       8.6         0.280      1.60
#> 2          14.7       7.9         0.280      1.60
#> 3          15.0       7.9         0.26       1.5 
#> 4          14.3       7.7         0.3        1.7 
#> # ... with 146 more rows
```

## Performance and Compability

This is an experimental package which I started developing with my own
use cases in mind. I tried to keep the effort low, which is why this
package *does not* internalize (read: copy) internal dplyr functions
(especially the ‘context internals’). This made it relatively easy to
develop the package without copying tons of dplyr code and overwritting
existing functions, such as `mutate` and other one-table verbs.

However, the downside is that not relying on {dplyr} internals has some
negative effects in terms of performance and compability.

In a nutshell this means:

  - The *over-across function family* in {dplyover} is somewhat slower
    than the original `dplyr::across`.
  - Although {dplyover} is designed to work in {dplyr}, some features
    and edge cases will not work correctly.

The good news is, that even without relying on {dplyr} internals most of
the original functionality can be replicated, and although being a bit
less performant, the current setup is optimized to fall not too far
behind in terms of speed.

Regarding compability, I have spent quite some time on testing the
functionality and most of the tests for `dplyr::across` could be
replicated successfully.

For more information on the performance and compability of {dplyover}
see the vignette “Performance and Compability”.

## History

I originally opened a [feature request on
GitHub](https://github.com/tidyverse/dplyr/issues/4834) to include a
very special case version of `over` (or to that time `mutate_over`) into
{dplyr}. The adivse then was to make this kind of functionality
available in a separate package. While I was working on this very
special case version of `over`, I realized that its more general use
case looks more like a `purrr::map` function for inside {dplyr} verbs
with different variants, which led me to the *over-across function
family*.

## Acknowledgements and Disclaimer

This package is not only an extention of {dplyr}. The main functions in
{dplyover} are directly derived and based on `dplyr::across()` (dplyr’s
license and copyrights apply). So if this package is working correctly,
all the credit should go to the dplyr team.

My own contribution merely consists of:

1.  removing the underlying dependencies on dplyr’s internal functions,
    and
2.  slightly changing `across`’ logic to make it work for vectors and a
    combination of two vectors and/or sets of columns.

By this I most defniitely introduced some bugs and edge cases which
won’t work, and in which case I am the only one to blame.
