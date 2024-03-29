% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/over.R
\name{over}
\alias{over}
\title{Apply functions to a list or vector in 'dplyr'}
\usage{
over(.x, .fns, ..., .names = NULL, .names_fn = NULL)
}
\arguments{
\item{.x}{An atomic vector or list to apply functions to. Alternatively a
<\code{\link[=selection_helpers]{selection helper}}> can be used to create
a vector.}

\item{.fns}{Functions to apply to each of the elements in \code{.x}. For
functions that expect variable names as input, the selected strings need to
be turned into symbols and evaluated. \code{dplyrover} comes with a genuine helper
function that evaluates strings as names \code{\link[=.]{.()}}.

Possible values are:
\itemize{
\item A function
\item A purrr-style lambda
\item A list of functions/lambdas
}

For examples see the example section below.

Note that, unlike \code{across()}, \code{over()} does not accept \code{NULL} as a
value to \code{.fns}.}

\item{...}{Additional arguments for the function calls in \code{.fns}.}

\item{.names}{A glue specification that describes how to name the output
columns. This can use \code{{x}} to stand for the selected vector element, and
\code{{fn}} to stand for the name of the function being applied. The default
(\code{NULL}) is equivalent to \code{"{x}"} for the single function case and
\code{"{x}_{fn}"} for the case where a list is used for \code{.fns}.

Note that, depending on the nature of the underlying object in \code{.x},
specifying \code{{x}} will yield different results:
\itemize{
\item If \code{.x} is an unnamed atomic vector, \code{{x}} will represent each value.
\item If \code{.x} is a named list or atomic vector, \code{{x}} will represent each name.
\item If \code{.x} is an unnamed list, \code{{x}} will be the index number running from 1 to \code{length(x)}.
}

This standard behavior (interpretation of \code{{x}}) can be overwritten by
directly specifying:
\itemize{
\item \code{{x_val}} for \code{.x}'s values
\item \code{{x_nm}} for its names
\item \code{{x_idx}} for its index numbers
}

Alternatively, a character vector of length equal to the number of columns to
be created can be supplied to \code{.names}. Note that in this case, the glue
specification described above is not supported.}

\item{.names_fn}{Optionally, a function that is applied after the glue
specification in \code{.names} has been evaluated. This is, for example, helpful
in case the resulting names need to be further cleaned or trimmed.}
}
\value{
A tibble with one column for each element in \code{.x} and each function in \code{.fns}.
}
\description{
\code{over()} makes it easy to create new colums inside a \code{\link[dplyr:mutate]{dplyr::mutate()}} or
\code{\link[dplyr:summarise]{dplyr::summarise()}} call by applying a function (or a set of functions) to
an atomic vector or list using a syntax similar to \code{\link[dplyr:across]{dplyr::across()}}.
The main difference is that \code{\link[dplyr:across]{dplyr::across()}} transforms or creates new columns
based on existing ones, while \code{over()} can create new columns based on a
vector or list to which it will apply one or several functions.
Whereas \code{\link[dplyr:across]{dplyr::across()}} allows \code{tidy-selection} helpers to select columns,
\code{over()} provides its own helper functions to select strings or values based
on either (1) values of specified columns or (2) column names. See the
examples below and the \code{vignette("why_dplyover")} for more details.
}
\section{Note}{

Similar to \code{dplyr::across()} \code{over()} works only inside dplyr verbs.
}

\section{Examples}{


It has two main use cases. They differ in how the elements in \code{.x}
are used. Let's first attach \code{dplyr}:\if{html}{\out{<div class="r">}}\preformatted{library(dplyr)

# For better printing
iris <- as_tibble(iris)
}\if{html}{\out{</div>}}
\subsection{(1) The General Use Case}{

Here the values in \code{.x} are used as inputs to one or more functions in \code{.fns}.
This is useful, when we want to create several new variables based on the same
function with varying arguments. A good example is creating a bunch of lagged
variables.\if{html}{\out{<div class="r">}}\preformatted{tibble(x = 1:25) \%>\%
  mutate(over(c(1:3),
              ~ lag(x, .x)))
#> # A tibble: 25 x 4
#>       x   `1`   `2`   `3`
#>   <int> <int> <int> <int>
#> 1     1    NA    NA    NA
#> 2     2     1    NA    NA
#> 3     3     2     1    NA
#> 4     4     3     2     1
#> # ... with 21 more rows
}\if{html}{\out{</div>}}

Lets create a dummy variable for each unique value in 'Species':\if{html}{\out{<div class="r">}}\preformatted{iris \%>\%
  mutate(over(unique(Species),
             ~ if_else(Species == .x, 1, 0)),
         .keep = "none")
#> # A tibble: 150 x 3
#>   setosa versicolor virginica
#>    <dbl>      <dbl>     <dbl>
#> 1      1          0         0
#> 2      1          0         0
#> 3      1          0         0
#> 4      1          0         0
#> # ... with 146 more rows
}\if{html}{\out{</div>}}

With \code{over()} it is also possible to create several dummy variables with
different thresholds. We can use the \code{.names} argument to control the output
names:\if{html}{\out{<div class="r">}}\preformatted{iris \%>\%
mutate(over(seq(4, 7, by = 1),
            ~ if_else(Sepal.Length < .x, 1, 0),
            .names = "Sepal.Length_\{x\}"),
         .keep = "none")
#> # A tibble: 150 x 4
#>   Sepal.Length_4 Sepal.Length_5 Sepal.Length_6 Sepal.Length_7
#>            <dbl>          <dbl>          <dbl>          <dbl>
#> 1              0              0              1              1
#> 2              0              1              1              1
#> 3              0              1              1              1
#> 4              0              1              1              1
#> # ... with 146 more rows
}\if{html}{\out{</div>}}

A similar approach can be used with dates. Below we loop over a date
sequence to check whether the date falls within a given start and end
date. We can use the \code{.names_fn} argument to clean the resulting output
names:\if{html}{\out{<div class="r">}}\preformatted{# some dates
dat_tbl <- tibble(start = seq.Date(as.Date("2020-01-01"),
                                   as.Date("2020-01-15"),
                                   by = "days"),
                  end = start + 10)

dat_tbl \%>\%
  mutate(over(seq(as.Date("2020-01-01"),
                  as.Date("2020-01-21"),
                  by = "weeks"),
              ~ .x >= start & .x <= end,
              .names = "day_\{x\}",
              .names_fn = ~ gsub("-", "", .x)))
#> # A tibble: 15 x 5
#>    start      end        day_20200101 day_20200108 day_20200115
#>    <date>     <date>     <lgl>        <lgl>        <lgl>       
#>  1 2020-01-01 2020-01-11 TRUE         TRUE         FALSE       
#>  2 2020-01-02 2020-01-12 FALSE        TRUE         FALSE       
#>  3 2020-01-03 2020-01-13 FALSE        TRUE         FALSE       
#>  4 2020-01-04 2020-01-14 FALSE        TRUE         FALSE       
#>  5 2020-01-05 2020-01-15 FALSE        TRUE         TRUE        
#>  6 2020-01-06 2020-01-16 FALSE        TRUE         TRUE        
#>  7 2020-01-07 2020-01-17 FALSE        TRUE         TRUE        
#>  8 2020-01-08 2020-01-18 FALSE        TRUE         TRUE        
#>  9 2020-01-09 2020-01-19 FALSE        FALSE        TRUE        
#> 10 2020-01-10 2020-01-20 FALSE        FALSE        TRUE        
#> 11 2020-01-11 2020-01-21 FALSE        FALSE        TRUE        
#> 12 2020-01-12 2020-01-22 FALSE        FALSE        TRUE        
#> 13 2020-01-13 2020-01-23 FALSE        FALSE        TRUE        
#> 14 2020-01-14 2020-01-24 FALSE        FALSE        TRUE        
#> 15 2020-01-15 2020-01-25 FALSE        FALSE        TRUE
}\if{html}{\out{</div>}}

\code{over()} can summarise data in wide format. In the example below, we want to
know for each group of customers (\code{new}, \code{existing}, \code{reactivate}), how much
percent of the respondents gave which rating on a five point likert scale
(\code{item1}). A usual approach in the tidyverse would be to use
\code{count \%>\% group_by \%>\% mutate}, which yields the same result in the usually
prefered long format. Sometimes, however, we might want this kind of summary
in the wide format, and in this case \code{over()} comes in handy:\if{html}{\out{<div class="r">}}\preformatted{csatraw \%>\%
  group_by(type) \%>\%
  summarise(over(c(1:5),
                 ~ mean(item1 == .x)))
#> # A tibble: 3 x 6
#>   type          `1`   `2`   `3`   `4`    `5`
#>   <chr>       <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1 existing   0.156  0.234 0.234 0.266 0.109 
#> 2 new        0.0714 0.268 0.357 0.214 0.0893
#> 3 reactivate 0.0667 0.267 0.133 0.4   0.133
}\if{html}{\out{</div>}}

Instead of a vector we can provide a named list of vectors to calculate the
top two and bottom two categories on the fly:\if{html}{\out{<div class="r">}}\preformatted{csatraw \%>\%
  group_by(type) \%>\%
  summarise(over(list(bot2 = c(1:2),
                      mid  = 3,
                      top2 = c(4:5)),
                 ~ mean(item1 \%in\% .x)))
#> # A tibble: 3 x 4
#>   type        bot2   mid  top2
#>   <chr>      <dbl> <dbl> <dbl>
#> 1 existing   0.391 0.234 0.375
#> 2 new        0.339 0.357 0.304
#> 3 reactivate 0.333 0.133 0.533
}\if{html}{\out{</div>}}

\code{over()} can also loop over columns of a data.frame. In the example below we
want to create four different dummy variables of \code{item1}: (i) the top and (ii)
bottom category as well as (iii) the top two and (iv) the bottom two categories.
We can create a lookup \code{data.frame} and use all columns but the first as input to
\code{over()}. In the function call we make use of base R's \code{match()}, where \code{.x}
represents the new values and \code{recode_df[, 1]} refers to the old values.\if{html}{\out{<div class="r">}}\preformatted{
recode_df <- data.frame(old  = c(1, 2, 3, 4, 5),
                        top1 = c(0, 0, 0, 0, 1),
                        top2 = c(0, 0, 0, 1, 1),
                        bot1 = c(1, 0, 0, 0, 0),
                        bot2 = c(1, 1, 0, 0, 0))

csatraw \%>\%
  mutate(over(recode_df[,-1],
              ~ .x[match(item1, recode_df[, 1])],
              .names = "item1_\{x\}")) \%>\%
  select(starts_with("item1"))
#> # A tibble: 150 x 6
#>   item1 item1_open item1_top1 item1_top2 item1_bot1 item1_bot2
#>   <dbl> <chr>           <dbl>      <dbl>      <dbl>      <dbl>
#> 1     3 12                  0          0          0          0
#> 2     2 22                  0          0          0          1
#> 3     2 21, 22, 23          0          0          0          1
#> 4     4 12, 13, 11          0          1          0          0
#> # ... with 146 more rows
}\if{html}{\out{</div>}}

\code{over()} work nicely with comma separated values stored in character vectors.
In the example below, the colum \code{csat_open} contains one or more comma
separated reasons why a specific customer satisfaction rating was given.
We can easily create a column for each response category with the help of
\code{dist_values} - a wrapper around \code{unique} which can split vector elements
using a separator:\if{html}{\out{<div class="r">}}\preformatted{csat \%>\%
  mutate(over(dist_values(csat_open, .sep = ", "),
              ~ as.integer(grepl(.x, csat_open)),
              .names = "rsp_\{x\}",
              .names_fn = ~ gsub("\\\\s", "_", .x)),
              .keep = "none") \%>\% glimpse
#> Rows: 150
#> Columns: 6
#> $ rsp_friendly_staff <int> 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0,~
#> $ rsp_good_service   <int> 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0,~
#> $ rsp_great_product  <int> 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0,~
#> $ rsp_no_response    <int> 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1,~
#> $ rsp_too_expensive  <int> 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0,~
#> $ rsp_unfriendly     <int> 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1,~
}\if{html}{\out{</div>}}
}

\subsection{(2) A Very Specific Use Case}{

Here strings are supplied to \code{.x} to construct column names (sharing the
same stem). This allows us to dynamically use more than one column in the
function calls in \code{.fns}. To work properly, the strings need to be
turned into symbols and evaluated. For this {dplyover} provides a genuine
helper function \code{.()} that evaluates strings and helps to declutter the
otherwise rather verbose code. \code{.()} supports glue syntax and takes a string
as argument.

Below are a few examples using two colums in the function calls in \code{.fns}.
For the two column case \code{\link[=across2]{across2()}} provides a more intuitive API that is
closer to the original \code{dplyr::across}. Using \code{.()} inside \code{over} is really
useful for cases with more than two columns.

Consider the following example of a purrr-style formula in \code{.fns} using \code{.()}:\if{html}{\out{<div class="r">}}\preformatted{iris \%>\%
  mutate(over(c("Sepal", "Petal"),
              ~ .("\{.x\}.Width") + .("\{.x\}.Length")
              ))
#> # A tibble: 150 x 7
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Sepal Petal
#>          <dbl>       <dbl>        <dbl>       <dbl> <fct>   <dbl> <dbl>
#> 1          5.1         3.5          1.4         0.2 setosa    8.6   1.6
#> 2          4.9         3            1.4         0.2 setosa    7.9   1.6
#> 3          4.7         3.2          1.3         0.2 setosa    7.9   1.5
#> 4          4.6         3.1          1.5         0.2 setosa    7.7   1.7
#> # ... with 146 more rows
}\if{html}{\out{</div>}}

The above syntax is equal to the more verbose:\if{html}{\out{<div class="r">}}\preformatted{iris \%>\%
  mutate(over(c("Sepal", "Petal"),
              ~ eval(sym(paste0(.x, ".Width"))) +
                eval(sym(paste0(.x, ".Length")))
              ))
#> # A tibble: 150 x 7
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Sepal Petal
#>          <dbl>       <dbl>        <dbl>       <dbl> <fct>   <dbl> <dbl>
#> 1          5.1         3.5          1.4         0.2 setosa    8.6   1.6
#> 2          4.9         3            1.4         0.2 setosa    7.9   1.6
#> 3          4.7         3.2          1.3         0.2 setosa    7.9   1.5
#> 4          4.6         3.1          1.5         0.2 setosa    7.7   1.7
#> # ... with 146 more rows
}\if{html}{\out{</div>}}

\code{.()} also works with anonymous functions:\if{html}{\out{<div class="r">}}\preformatted{iris \%>\%
  summarise(over(c("Sepal", "Petal"),
                function(x) mean(.("\{x\}.Width"))
                ))
#> # A tibble: 1 x 2
#>   Sepal Petal
#>   <dbl> <dbl>
#> 1  3.06  1.20
}\if{html}{\out{</div>}}

A named list of functions:\if{html}{\out{<div class="r">}}\preformatted{iris \%>\%
  mutate(over(c("Sepal", "Petal"),
              list(product = ~ .("\{.x\}.Width") * .("\{.x\}.Length"),
                   sum = ~ .("\{.x\}.Width") + .("\{.x\}.Length"))
                   ),
         .keep = "none")
#> # A tibble: 150 x 4
#>   Sepal_product Sepal_sum Petal_product Petal_sum
#>           <dbl>     <dbl>         <dbl>     <dbl>
#> 1          17.8       8.6          0.28       1.6
#> 2          14.7       7.9          0.28       1.6
#> 3          15.0       7.9          0.26       1.5
#> 4          14.3       7.7          0.3        1.7
#> # ... with 146 more rows
}\if{html}{\out{</div>}}

Again, use the \code{.names} argument to control the output names:\if{html}{\out{<div class="r">}}\preformatted{iris \%>\%
  mutate(over(c("Sepal", "Petal"),
              list(product = ~ .("\{.x\}.Width") * .("\{.x\}.Length"),
                   sum = ~ .("\{.x\}.Width") + .("\{.x\}.Length")),
              .names = "\{fn\}_\{x\}"),
         .keep = "none")
#> # A tibble: 150 x 4
#>   product_Sepal sum_Sepal product_Petal sum_Petal
#>           <dbl>     <dbl>         <dbl>     <dbl>
#> 1          17.8       8.6          0.28       1.6
#> 2          14.7       7.9          0.28       1.6
#> 3          15.0       7.9          0.26       1.5
#> 4          14.3       7.7          0.3        1.7
#> # ... with 146 more rows
}\if{html}{\out{</div>}}
}
}

\seealso{
\code{\link[=over2]{over2()}} to apply a function to two objects.

All members of the <\code{\link[=over_across_family]{over-across function family}}>.
}
