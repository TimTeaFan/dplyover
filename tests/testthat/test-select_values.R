# select_values  ------------------------------------------------------------------
# select_values examples of basic functionality from the example section
library(dplyr)

## examples
test_that("use case: dist_values() in over()", {

  over_dist_val <- iris %>%
    mutate(over(dist_values(Species),
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

test_that("dist_values() short examples", {

  expect_equal(dist_values(c(1:3, NA)), c(1:3))

  expect_equal(dist_values(c(1:3, NA), .sort = "desc"), c(3:1))

  expect_equal(dist_values(c(3, 1, 2, NA), .sort = "none"), c(3, 1, 2))

  expect_equal(
    factor(c(1:3, NA)) %>%
    as.factor() %>%
    dist_values() %>%
    class(), "character")

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


test_that("dist_values() works with comma separated vectors and lists", {

  expect_equal(
    c("1, 2, 3",
      "2, 4, 5, 6",
      "4, 1, 7") %>%
      dist_values(., .sep = ", "),
    as.character(1:7)
  )

  expect_equal(
    list(a = c(1:4), b = (4:6), c(5:10)) %>%
      dist_values(),
    c(1:10))

})


test_that("dist_values() example with factors", {

  fctrs <- factor(letters[1:3], levels = c("b", "a", "c"))

  expect_equal(dist_values(fctrs), c("b", "a", "c"))

  expect_equal(dist_values(fctrs, .sort = "asc"), letters[1:3])

  expect_equal(dist_values(fctrs, .sort = "desc"), letters[3:1])

  expect_equal(dist_values(fctrs, .sort = "none"), letters[1:3])

})

# more tests

test_that("seq_range() error check", {

  expect_error(seq_range(letters[1:3], 1))

})


