# select_values  ------------------------------------------------------------------
# select_values examples of basic functionality from the example section
library(dplyr)

## examples
test_that("use case: unique_tidy() in over()", {

  over_dist_val <- iris %>%
    mutate(over(unique_tidy(Species),
                ~ if_else(Species == .x, 1, 0)
    ),
    .keep = "none")

  df_expect <- iris %>%
    mutate(setosa = if_else(Species == "setosa", 1, 0),
           versicolor = if_else(Species == "versicolor", 1, 0),
           virginica = if_else(Species == "virginica", 1, 0),
           .keep = "none")

  expect_equal(over_dist_val, df_expect)

})

test_that("unique_tidy() supports tidyselect syntax", {

  over_dist_val <- mtcars %>%
    mutate(over(unique_tidy(vs:am),
                ~ if_else(mpg == .x, 1, 0)
    ),
    .keep = "none")

  df_expect <- mtcars %>%
    mutate(`0` = 0,
           `1` = 0,
           .keep = "none")

  expect_equal(over_dist_val, df_expect)

})

test_that("unique_tidy() short examples", {

  expect_equal(unique_tidy(c(1:3, NA), sort = "asc"), c(1:3))

  expect_equal(unique_tidy(c(1:3, NA), sort = "desc"), c(3:1))

  expect_equal(unique_tidy(c(3, 1, 2, NA), sort = "none"), c(3, 1, 2))

  expect_equal(
    factor(c(1:3, NA)) %>%
    as.factor() %>%
    unique_tidy() %>%
    class(), "character")

})

test_that("unique_tidy() works with comma separated vectors and lists", {

  expect_equal(
    c("1, 2, 3",
      "2, 4, 5, 6",
      "4, 1, 7") %>%
      unique_tidy(., sep = ", "),
    as.character(1:7)
  )

  expect_equal(
    list(a = c(1:4), b = (4:6), c(5:10)) %>%
      unique_tidy(),
    c(1:10))

})


test_that("unique_tidy() example with factors", {

  fctrs <- factor(letters[1:3], levels = c("b", "a", "c"))

  expect_equal(unique_tidy(fctrs), c("b", "a", "c"))

  expect_equal(unique_tidy(fctrs, sort = "none"), c("b", "a", "c"))

  expect_equal(unique_tidy(fctrs, sort = "asc"), letters[1:3])

  expect_equal(unique_tidy(fctrs, sort = "desc"), letters[3:1])



})

test_that("unique_tidy() `grp_data` argument works as expected", {

  # warn character
  expect_warning({
    iris %>%
      mutate(Species = as.character(Species)) %>%
      group_by(Species) %>%
      mutate(over(unique_tidy(Species),
                   ~ if_else(Species == .x, 1, 0)))
    })

  # warn factor
  expect_warning({
    iris %>%
      group_by(Species) %>%
      mutate(over(unique_tidy(Species),
                  ~ if_else(Species == .x, 1, 0)))
  })

  # silent
  expect_warning({
    iris %>%
      group_by(Species) %>%
      mutate(over(unique_tidy(Species, grp_data = "silent"),
                  ~ if_else(Species == .x, 1, 0)))
  }, NA)

  # ungroup
  expect_equal(
    {iris %>%
      mutate(Species = as.character(Species)) %>%
      group_by(Species) %>%
      transmute(over(unique_tidy(Species, grp_data = "ungroup"),
                     ~ .x)) %>%
      ncol()},
    4L)
})

test_that("unique_tidy() supports tidy select in dplyr without dplyover", {
  expect_equal({
    mtcars %>%
      mutate(values = list(unique_tidy(am:gear))) %>%
      slice(1) %>%
      pull(values) %>%
      unlist},
    c(0, 1, 3, 4, 5)
    )
})


test_that("unique_tidy() `na.rm and `grp_data` arguments work as expected", {

  expect_equal({
    csat %>%
      transmute(over(unique_tidy(email_rating,
                                 na.rm = FALSE),
                     ~ if_else(same_value(email_rating, .x), 1, 0))) %>%
      ncol()
  }, 6L)

  expect_equal({
    csat %>%
      mutate(email_rating = as.character(email_rating)) %>%
      transmute(over(unique_tidy(email_rating,
                                 na.rm = FALSE),
                     ~ if_else(same_value(email_rating, .x), 1, 0))) %>%
      ncol()
  }, 6L)

  expect_equal({
    csat %>%
      group_by(email_rating) %>%
      transmute(over(unique_tidy(email_rating,
                                 grp_data = "ungroup",
                                 na.rm = FALSE),
                     ~ if_else(same_value(email_rating, .x), 1, 0))) %>%
      ncol()
    }, 7L)

})

test_that("use case: seq_rang() in over()", {

  over_seq_rang <- iris %>%
    mutate(over(seq_range(Sepal.Length, 1),
                ~ if_else(Sepal.Length > .x, 1, 0),
                .names = "Sepal.Length.{x}"),
           .keep = "none")

  df_expect <- iris %>%
    mutate(Sepal.Length.5 = if_else(Sepal.Length > 5, 1, 0),
           Sepal.Length.6 = if_else(Sepal.Length > 6, 1, 0),
           Sepal.Length.7 = if_else(Sepal.Length > 7, 1, 0),
           .keep = "none")

  expect_equal(over_seq_rang, df_expect)

})

test_that("seq_rang() on dates", {

  some_dates <- c(as.Date("2020-01-02"),
                  as.Date("2020-05-02"),
                  as.Date("2020-03-02"))



  expect_equal(seq_range(some_dates, "1 month"),
               as.Date(c("2020-01-02",
                         "2020-02-02",
                         "2020-03-02",
                         "2020-04-02",
                         "2020-05-02")))

})


# more tests

test_that("seq_range() error check", {

  expect_error(seq_range(letters[1:3], 1))

})


