# crossover ------------------------------------------------------------------
library(dplyr)

# crossover examples of basic functionality from the example section
test_that("crossover() exmample with function names in `.y` ", {

  df_crossover <- iris %>%
        summarise(
          crossover(starts_with("sepal"),
                    c("mean", "sd"),
                    ~ do.call(.y, list(.x)),
                    .names = "{xcol}_{y}"))

  df_expect <- iris %>%
    summarise(Sepal.Length_mean = mean(Sepal.Length),
              Sepal.Width_sd = sd(Sepal.Width))

  expect_equal(df_crossover, df_expect)

})

test_that("crossoverx() exmample lagged variables", {

  df_crossoverx <- iris %>%
    transmute(crossoverx(starts_with("sepal"),
                        1:5,
                        list(lag = ~ lag(.x, .y)),
                        .names = "{xcol}_{fn}{y}"))

  df_expect <- iris %>%
    transmute(Sepal.Length_lag1 = lag(Sepal.Length, 1),
              Sepal.Length_lag2 = lag(Sepal.Length, 2),
              Sepal.Length_lag3 = lag(Sepal.Length, 3),
              Sepal.Length_lag4 = lag(Sepal.Length, 4),
              Sepal.Length_lag5 = lag(Sepal.Length, 5),
              Sepal.Width_lag1 = lag(Sepal.Width, 1),
              Sepal.Width_lag2 = lag(Sepal.Width, 2),
              Sepal.Width_lag3 = lag(Sepal.Width, 3),
              Sepal.Width_lag4 = lag(Sepal.Width, 4),
              Sepal.Width_lag5 = lag(Sepal.Width, 5))

  expect_equal(df_crossoverx, df_expect)

})

# tests adopted from across
test_that("crossoverx() works on one column data.frame", {

  df0 <- data.frame(x = 1)

  df_crossoverx <- df0 %>%
    mutate(crossoverx(everything(), 1, ~ .x * .y))

  df_expect <- df0 %>%
    mutate(`x_1` = x * 1)

  expect_equal(df_crossoverx, df_expect)

})

test_that("crossoverx() does not select grouping variables", {

  df0 <- data.frame(g = 1, x = 1)

  df_crossoverx <- df0 %>%
    group_by(g) %>%
    summarise(x = crossoverx(everything(), 1, ~ .x * .y)) %>%
    pull()

  expect_equal(df_crossoverx, tibble(`x_1` = 1))

})

test_that("crossoverx() correctly names output columns", {
  df <- tibble(x = 1, y = 2, z = 3, s = "")
  gf <- group_by(df, x)

  expect_named(
    mutate(gf, crossoverx(c(y,z), 3:4, ~ .x * .y)),
    c("x", "y", "z", "s", "y_3", "y_4", "z_3", "z_4")
  )
  expect_named(
    mutate(gf, crossoverx(c(y,z), 3:4, ~ .x * .y, .names = "id_{xcol}_{y}")),
    c("x", "y", "z", "s", "id_y_3", "id_y_4", "id_z_3", "id_z_4")
  )
  expect_named(
    summarise(gf, crossoverx(c(y,z), 3:4, ~ mean(.x + .y), .names = "mean_{xcol}_{y}")),
    c("x", "mean_y_3", "mean_y_4", "mean_z_3", "mean_z_4")
  )
  expect_named(
    summarise(gf, crossoverx(c(y,z), 3:4, list(paste = paste, sum = sum))),
    c("x", "y_3_paste", "y_3_sum", "y_4_paste", "y_4_sum", "z_3_paste", "z_3_sum",  "z_4_paste", "z_4_sum")
  )
  expect_named(
    summarise(gf, crossoverx(c(y,z), 3:4, list(paste = paste, mean))),
    c("x", "y_3_paste", "y_3_2", "y_4_paste", "y_4_2", "z_3_paste", "z_3_2", "z_4_paste", "z_4_2")
  )
  expect_named(
    summarise(gf, crossoverx(c(y,z), 3:4, list(paste, mean = mean))),
    c("x", "y_3_1", "y_3_mean", "y_4_1", "y_4_mean", "z_3_1", "z_3_mean", "z_4_1", "z_4_mean")
  )
  expect_named(
    summarise(gf, crossoverx(c(y,z), 3:4, list(mean, sum))),
    c("x", "y_3_1", "y_3_2", "y_4_1", "y_4_2", "z_3_1", "z_3_2", "z_4_1", "z_4_2")
  )
  expect_named(
    summarise(gf, crossoverx(c(y,z),
                            3:4,
                            list(mean = mean, paste = paste),
                            .names = "{fn}_{xcol}_{y}")),
    c("x", "mean_y_3", "paste_y_3", "mean_y_4", "paste_y_4", "mean_z_3", "paste_z_3", "mean_z_4", "paste_z_4")
  )
  # further added crossoverx()'s y_val, y_idx, y_nm
  expect_named(
    summarise(gf, crossoverx(c(y,z),
                             list(a = 3, b = 4),
                             list(mean = mean, paste = paste),
                             .names = "{fn}_{xcol}_{y_val}")),
    c("x", "mean_y_3", "paste_y_3", "mean_y_4", "paste_y_4", "mean_z_3", "paste_z_3", "mean_z_4", "paste_z_4")
  )
  expect_named(
    summarise(gf, crossoverx(c(y,z),
                            list(a = 3, b = 4),
                            list(mean = mean, paste = paste),
                            .names = "{fn}_{xcol}_{y_nm}")),
    c("x", "mean_y_a", "paste_y_a", "mean_y_b", "paste_y_b", "mean_z_a", "paste_z_a", "mean_z_b", "paste_z_b")
  )
  expect_warning(
    summarise(gf, crossoverx(c(y,z),
                            list(a = 3:4, b = 5),
                            list(mean = ~ mean(.x + .y), sum = ~ sum(.x + .y)),
                            .names = "{fn}_{xcol}_{y_val}"))
  )

  expect_warning(
    summarise(gf, crossoverx(c(y,z),
                            list(3, 4),
                            list(mean = ~ mean(.x + .y), sum = ~ sum(.x + .y)),
                            .names = "{fn}_{xcol}_{y_nm}"))
  )
  expect_named(
    summarise(gf, crossoverx(c(y,z),
                            list(3, 4),
                            list(sum = sum))),
    c("x", "y_1_sum", "y_2_sum", "z_1_sum", "z_2_sum")
  )
  expect_named(
    summarise(gf, crossoverx(c(y,z),
                            list(a = 3, b = 4),
                            list(mean = mean, paste = paste),
                            .names = "{fn}_{xcol}_{y_idx}")),
    c("x", "mean_y_1", "paste_y_1", "mean_y_2", "paste_y_2", "mean_z_1", "paste_z_1", "mean_z_2", "paste_z_2")
  )
  expect_error(
    summarise(gf, crossoverx(c(y,z),
                            list(z = 3, y = 4),
                            list(mean = mean, paste = paste),
                            .names = "{xcol}_{y_idx}"))
  )

  # further added external vector
  col_nm_vec <- c("one", "two", "three", "four", "five", "six", "seven", "eight")
  expect_named(
    summarise(gf, crossoverx(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = col_nm_vec)),
    c("x", col_nm_vec)
  )
  # test that external vector throws error when too short
  col_nm_vec2 <- c("one", "two", "three", "four")
  expect_error(
    summarise(gf, crossoverx(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = col_nm_vec2))
  )
  # test that external vector throws error when too long
  col_nm_vec3 <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  expect_error(
    summarise(gf, crossoverx(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = col_nm_vec3))
  )
  expect_error(
    summarise(gf, crossoverx(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = "new"))
  )
  # test that external vectors throws error when it contains non-unique names
  col_nm_vec4 <- rep(c("one", "two", "three", "four"), 2)
  expect_error(
    summarise(gf, crossoverx(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = col_nm_vec4))
  )
  # test external names vector with function in .y
  # col_nm_vec <- c("one", "two")
  # expect_named(
  #   summarise(df, crossoverx(c(y,z),
  #                           dist_values,
  #                           ~ if_else(.x == .y, 1L, 0L),
  #                           .names = col_nm_vec)),
  #   col_nm_vec)
  # error case for fns in .y
  # col_nm_vec <- c("one", "two", "three")
  # expect_error(
  #   summarise(df, crossoverx(c(y,z),
  #                           dist_values,
  #                           ~ if_else(.x == .y, 1L, 0L),
  #                           .names = col_nm_vec)))

})


test_that("crossoverx() result locations are aligned with .fn list names", {

  df <- tibble(x = 1:2, y = 3:4)

  df_crossoverx <- summarise(df,
                            crossoverx(c(x, y), 3:4,
                                      list(cls = ~ class(.x + .y),
                                           type = ~ is.numeric(.x + .y))))

  expect <- tibble(`x_3_cls` = "integer", `x_3_type` = TRUE,
                    `x_4_cls` = "integer", `x_4_type` = TRUE,
                    `y_3_cls` = "integer", `y_3_type` = TRUE,
                    `y_4_cls` = "integer", `y_4_type` = TRUE)

  expect_identical(df_crossoverx, expect)

})


test_that("crossoverx() passes ... to functions", {

  df <- tibble(x = 1, y = 2)

  expect_equal(
    summarise(df, crossoverx(c(x, y),
                            list(a = 10, b = 20),
                            sum,
                            na.rm = TRUE)),
    tibble(x_a = 11, x_b = 21, y_a = 12, y_b = 22)
  )

  mean2 <- function(x, y, ...) {
    mean(c(x,y), ...)
  }

  expect_equal(
    summarise(df, crossoverx(c(x, y),
                            list(a = 10, b = NA),
                           list(sum = sum, mean = mean2), na.rm = TRUE)),
    tibble(x_a_sum = 11, x_a_mean = 5.5,
           x_b_sum = 1, x_b_mean = 1,
           y_a_sum = 12, y_a_mean = 6,
           y_b_sum = 2, y_b_mean = 2)
  )

})

test_that("crossoverx() passes unnamed arguments following .fns as ...", {

  df <- tibble(x = 1, y = "b")

  expect_equal(mutate(df, crossoverx(c(x, y), c(3, NA), paste, "a")),
               tibble(x = 1, y = "b",
                      x_3 = "1 3 a", x_NA = "1 NA a",
                      y_3 = "b 3 a", y_NA = "b NA a"))
})

# test_that("over() works sequentially", {
#
#   df <- tibble(a = 1)
#
#   expect_equal(
#     mutate(df,
#            x = ncol(over(1, mean)),
#            y = ncol(over(1:2, mean))),
#     tibble(a = 1, x = 1L, y = 2L)
#   )
#
#   expect_equal(
#     mutate(df,
#            a = "x",
#            y = ncol(over(1, mean))),
#     tibble(a = "x", y = 1L)
#   )
#
#   expect_equal(
#     mutate(df,
#            x = 1,
#            y = ncol(over(1:2, mean))),
#     tibble(a = 1, x = 1, y = 2L)
#   )
# })

test_that("crossoverx() retains original ordering", {
  df <- tibble(a = c(1:2), b = c(3:4))

  expect_equal(mutate(df, a = c(5:6), x = crossoverx(c(a, b), .data$b, sum))$x,
               tibble(a_3 = c(14, 14), a_4 = c(15, 15), b_3 = c(10, 10), b_4 = c(11, 11)))

})

# test_that("crossoverx() gives meaningful messages", {
#
  # # only over2, over2x specific error messages go here
  # gf <- tibble(g = 1, x = 1)
  #
  # expect_snapshot_error(
  #   summarise(gf,
  #             crossoverx(c(y,z),
  #                       dist_values,
  #                       ~ if_else(.x == .y, 1L, 0L)))
  #   )

# })

test_that("crossoverx() uses environment from the current quosure (#5460)", {
  # If the data frame `y` is selected, causes a subscript conversion
  # error since it is fractional

  df <- data.frame(x = c(1, 2), y = c(1.1, 2.4))
  y <- "x"

  expect_equal(df %>%
                 summarise(crossoverx(all_of(y),
                                     1,
                                     ~ mean(.x, na.rm = .y))),
               data.frame(x_1 = 1.5))

  expect_equal(df %>% filter(crossoverx(all_of(y), 1, ~ .x + .y <= 2)),
               slice(df, 1))

  # Recursive case fails because the `y` column has precedence (across issue: #5498)
  # expect_error(df %>% summarise(summarise(across(), across(all_of(y), mean))))

  # Inherited case
  # doesn't work in testthat or in reprex, but works locally, that's fair enough:
  # out <- df %>% summarise(local(crossoverx(all_of(y),
  #                                         1,
  #                                         ~ mean(.x, na.rm = .y))))
  # expect_equal(out, data.frame(x_1 = 1.5))

  # Related test: nested case without `local`
  expect_equal(mutate(df, tibble(new = list(crossoverx(x, 1,
                                                      ~ sum(c(.x, .y)))))),
               mutate(df, new = list(tibble(x_1 = 4)))
  )
})



# expected errors

test_that("crossoverx() custom errors", {

  # inside dplyr
  expect_error(crossoverx())

  # .fns must be function
  expect_error(
    summarise(tibble(x = 1), crossoverx(x, 2, 42))
  )

  # check keep used
  expect_warning(
    mutate(tibble(x = 1), crossoverx(x, 2, mean), .keep = "used"),
    "does not support the `.keep`"
  )

  # check keep unused
  expect_warning(
    mutate(tibble(x = 1), crossoverx(x, 2, mean), .keep = "unused"),
    "does not support the `.keep`"
  )

  # .y is function does not work on grouped df
  # gf <- tibble(g = 1, x = 1)
  # expect_error(summarise(gf,
  #                        crossoverx_old(c(y,z),
  #                                      dist_values,
  #                                      ~ if_else(.x == .y, 1L, 0L))))

})

# other edge cases
