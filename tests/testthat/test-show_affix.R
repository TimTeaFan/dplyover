# show_affix  ------------------------------------------------------------------

library(dplyr)

# select_vars examples of basic functionality from the example section

## examples
test_that("show_affix can be called after across2 error", {

  expect_error({

    iris %>%
     as_tibble %>%
     rename("Pesal.Length" = Sepal.Length) %>%
     mutate(across2(ends_with("Length"),
                    ends_with("Width"),
                    .fns = list(product = ~ .x * .y,
                                sum = ~ .x + .y),
                    .names = "{pre}_{fn}"))
    })

  out <- show_prefix()

  expected <- tibble(
    .xcols = c("Pesal.Length", "Petal.Length"),
    .ycols = c("Sepal.Width", "Petal.Width"),
    prefix = c(NA_character_, "Petal")
    )

  expect_equal(out, expected)

  expect_error({

    iris %>%
      as_tibble %>%
      rename("Sepal.thengl" = Sepal.Length) %>%
      mutate(across2(starts_with("Petal"),
                     starts_with("Sepal"),
                     .fns = list(product = ~ .x * .y,
                                 sum = ~ .x + .y),
                     .names = "{suf}_{fn}"))
  })

  out2 <- show_suffix()

  expected2 <- tibble(
    .xcols = c("Petal.Length", "Petal.Width"),
    .ycols = c("Sepal.thengl", "Sepal.Width"),
    suffix = c(NA_character_, "Width")
  )

  expect_equal(out2, expected2)

})

test_that("show_suffix can called on a data.frame", {

  out <-  iris %>%
            show_suffix(starts_with("Sepal"),
                        starts_with("Petal"))

  expected <- tibble(
    .xcols = c("Sepal.Length", "Sepal.Width"),
    .ycols = c("Petal.Length", "Petal.Width"),
    suffix = c("Length", "Width")
  )

  expect_equal(out, expected)

  out2 <-  iris %>%
    show_prefix(ends_with("Length"),
                ends_with("Width"))

  expected2 <- tibble(
    .xcols = c("Sepal.Length", "Petal.Length"),
    .ycols = c("Sepal.Width", "Petal.Width"),
    prefix = c("Sepal", "Petal")
  )

  expect_equal(out2, expected2)

})


