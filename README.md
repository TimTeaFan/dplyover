
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dplyover

<!-- badges: start -->

[![CodeFactor](https://www.codefactor.io/repository/github/timteafan/dplyover/badge)](https://www.codefactor.io/repository/github/timteafan/dplyover)
[![Codecov test
coverage](https://codecov.io/gh/TimTeaFan/dplyover/branch/main/graph/badge.svg)](https://codecov.io/gh/TimTeaFan/dplyover?branch=main)
<!-- badges: end -->

## Overview

{dplyover} extends {dplyr}’s functionality by building a function family
around `dplyr::across()`.

The goal of this <over-across function family> is to provide a concise
and uniform syntax which can be used to create columns by applying
functions to vectors and / or sets of columns in {dplyr}. Ideally, this
will:

  - *improve our mental model* so that it is easier to tackle problems
    where the solution is based on creating new columns.
  - *reduce the amount of code* to create variables derived from
    existing colums when doing explanatory data analysis (EDA)
    (e.g. lagging, collapsing many variables in a similar way).
  - *provide a clean {dplyr} approach* to create many variables which
    are calculated based on two or more varialbes.

The functions in the <over-apply function family> create columns by
applying one or several functions to:

### basic functions

  - `dplyr::across()`: a set of columns (not part of {dplyover})
  - `over()`: a vector (list or atomic vector)

### variants

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
# dplyover is extention of dplyr:
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.0.2
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
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
variables. Below use column ‘a’ to create lag and lead variables by `1`,
`2` and `3` positions. `over()`’s `.names` argument lets us put nice
names on the output columns.

``` r
tibble(a = 1:10) %>%
  mutate(over(c(1:3),
              list(lag  = ~ lag(a, .x),
                   lead = ~ lead(a, .x)),
              .names = "a_{fn}{x}"))
#> # A tibble: 10 x 7
#>        a a_lag1 a_lead1 a_lag2 a_lead2 a_lag3 a_lead3
#>    <int>  <int>   <int>  <int>   <int>  <int>   <int>
#>  1     1     NA       2     NA       3     NA       4
#>  2     2      1       3     NA       4     NA       5
#>  3     3      2       4      1       5     NA       6
#>  4     4      3       5      2       6      1       7
#>  5     5      4       6      3       7      2       8
#>  6     6      5       7      4       8      3       9
#>  7     7      6       8      5       9      4      10
#>  8     8      7       9      6      10      5      NA
#>  9     9      8      10      7      NA      6      NA
#> 10    10      9      NA      8      NA      7      NA
```

#### Applying a function with a varying argument to a set of columns

`crossoverx()` applies the functions in `.fns` to every combination of
colums in `.xcols` and elements in `.y`. This is similar to the example
above, but this time, we use a set of columns. Below we create five
lagged variables for each ‘Sepal.Length’ and ‘Sepal.Width’. To create
nice names we use a named list as argument in `.fns` and specify the
glue syntax in `.names.`

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
#> $ Sepal.Length_lag1 <dbl> NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.…
#> $ Sepal.Length_lag2 <dbl> NA, NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4…
#> $ Sepal.Length_lag3 <dbl> NA, NA, NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0,…
#> $ Sepal.Length_lag4 <dbl> NA, NA, NA, NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, …
#> $ Sepal.Length_lag5 <dbl> NA, NA, NA, NA, NA, 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4…
#> $ Sepal.Width_lag1  <dbl> NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.…
#> $ Sepal.Width_lag2  <dbl> NA, NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9…
#> $ Sepal.Width_lag3  <dbl> NA, NA, NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4,…
#> $ Sepal.Width_lag4  <dbl> NA, NA, NA, NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, …
#> $ Sepal.Width_lag5  <dbl> NA, NA, NA, NA, NA, 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3…
```

#### Applying functions to a pair of variables

`across2()` can be used to transform pairs of variables in one or more
functions. In the example below we want to calculate the product and the
sum of all pairs of ‘Length’ and ‘Width’ variables. We can use `{pre}`
in the glue specification in `.names` to extract the common prefix of
each pair of variables. We can further transform the names, in the
example setting them `tolower` by specifying the `.names_fn` argument:

``` r
iris %>%
  transmute(across2(ends_with("Length"),
                    ends_with("Width"),
                    .fns = list(product = ~ .x * .y,
                                sum = ~ .x + .y),
                   .names = "{pre}_{fn}",
                   .names_fn = tolower))
#> # A tibble: 150 x 4
#>    sepal_product sepal_sum petal_product petal_sum
#>            <dbl>     <dbl>         <dbl>     <dbl>
#>  1          17.8       8.6         0.280      1.60
#>  2          14.7       7.9         0.280      1.60
#>  3          15.0       7.9         0.26       1.5 
#>  4          14.3       7.7         0.3        1.7 
#>  5          18         8.6         0.280      1.60
#>  6          21.1       9.3         0.68       2.1 
#>  7          15.6       8           0.42       1.7 
#>  8          17         8.4         0.3        1.7 
#>  9          12.8       7.3         0.280      1.60
#> 10          15.2       8           0.15       1.6 
#> # … with 140 more rows
```

## Performance and Compability

This is an experimental package which I started developing with my own
use cases in mind. I tried to keep the effort low, which is why this
package *does not* internalize (read: copy) internal dplyr functions
(especially the ‘context internals’). This made it relatively easy to
develop the package without copying tons of dplyr code and overwritting
existing functions, such as `mutate` and other one table verbs.

However, the downside is that not relying on {dplyr} internals has some
negative effects in terms of performance and compability.

In a nutshell this means:

  - the over-across function family in {dplyover} is somewhat slower
    than the original `dplyr::across`.
  - Although {dplyover} is designed to work (only) in {dplyr}, some
    features and edge cases will not work correctly.

The good news is, that even without relying on {dplyr} internals most of
the original functionality can be replicated, and although being a bit
less performant, the current setup is optimized to fall not too far
behind in terms of speed.

Regarding compability, I have spent a serious amount of time on testing
by replicating most of the tests for `dplyr::across` successfully.

For more information on the performance and compability of {dplyover}
see the vignette “Performance and Compability”.

## History

I originally opened a [feature request on
GitHub](https://github.com/tidyverse/dplyr/issues/4834) to include a
very special case version of `over` (or to that time `mutate_over`) into
{dplyr}. The adivse then was to make this kind of functionality
available in a separate package. While I was working on this very
special case version of `over` I realized that the general use case
looks more like a `purrr::map` function for inside {dplyr} verbs with
different variants, which led me to the over-across function family.

## Acknowledgements and Disclaimer

This package is not only an extention of {dplyr}. The main functions in
{dplyover} are directly derived and based on `dplyr::across()` (dplyr’s
license and copyrights apply). So if this package is working correctly,
all the credit should go to the dplyr team.

My own contribution merely consists of:

1.  removing the underlying dependencies on dplyr’s internal functions,
    and
2.  slightly changing `across`’ logic to make it work for vectors and or
    a combination of two vectors and / or sets columns.

By this I probably managed to introduce some bugs and edge cases which
won’t work, and in which case I am the only one to blame.
