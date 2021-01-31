# string_eval  ------------------------------------------------------------------
# string_eval examples of basic functionality from the example section
library(dplyr)

## examples
test_that(".() works in over()", {

  df_over <- iris %>%
    mutate(over(c("Sepal", "Petal"),
                ~ .("{.x}.Width") + .("{.x}.Length")
    ))

  df_expect <- iris %>%
    mutate(
      Sepal = Sepal.Width + Sepal.Length,
      Petal = Petal.Width + Petal.Length
    )

  expect_equal(df_over, df_expect)

})

# add non-dplyover examples? (across, map)

## compability

# works locally but not in test_check
# test_that("data.table's .() still works", {
#
#   expect_error({
#     library(data.table)
#     mtcarsDT <- as.data.table(mtcars)
#     mtcarsDT[, .(mpg, hp)]
#     detach("package:data.table", unload = TRUE)
#   }, NA)
#
# })


test_that("magrittrs . still works", {

  expect_error(mtcars %>% nrow(.), NA)

})
