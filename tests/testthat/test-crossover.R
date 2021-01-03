# crossover ------------------------------------------------------------------
library(dplyr)

# crossover examples of basic functionality from the example section
test_that("crossover() exmample with `.y` as vector", {

  df_crossover <-  iris %>%
    mutate(crossover(starts_with("Sepal"),
                     c(5,3),
                     ~ .x <= .y))

  df_expect <- iris %>%
    mutate(Sepal.Length_5 = Sepal.Length <= 5,
           Sepal.Width_3 = Sepal.Width <= 3)

  expect_equal(df_crossover, df_expect)

})

test_that("crossover() exmample with `.y` as function", {

  df_crossover <- csat %>%
    transmute(
      crossover(.xcols = c(type, product, csat),
                .y = dist_values,
                .fns = ~ if_else(.y == .x, 1, 0),
                .names_fn = ~ gsub("\\s", "_", .x) %>% tolower(.)
      ))

  df_expect <- csat %>%
    transmute(type_existing = if_else(type == "existing", 1, 0),
           type_new = if_else(type == "new", 1, 0),
           type_reactivate = if_else(type == "reactivate", 1, 0),
           product_advanced = if_else(product == "advanced", 1, 0),
           product_basic = if_else(product == "basic", 1, 0),
           product_premium = if_else(product == "premium", 1, 0),
           csat_neutral = if_else(csat == "Neutral", 1, 0),
           csat_satisfied = if_else(csat == "Satisfied", 1, 0),
           csat_unsatisfied = if_else(csat == "Unsatisfied", 1, 0),
           csat_very_satisfied = if_else(csat == "Very satisfied", 1, 0),
           csat_very_unsatisfied = if_else(csat == "Very unsatisfied", 1, 0))

  expect_equal(df_crossover, df_expect)

})

test_that("crossoverx() exmample", {

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
test_that("crossover() works on one column data.frame", {

  df0 <- data.frame(x = 1)

  df_crossover <- df0 %>%
    mutate(crossover(everything(), 1, ~ .x * .y))

  df_expect <- df0 %>%
    mutate(`x_1` = x * 1)

  expect_equal(df_crossover, df_expect)

  df_crossoverx <- df0 %>%
    mutate(crossover(everything(), 1, ~ .x * .y))

  expect_equal(df_crossoverx, df_expect)

})

test_that("crossover() does not select grouping variables", {

  df0 <- data.frame(g = 1, x = 1)

  df_crossover <- df0 %>%
    group_by(g) %>%
    summarise(x = crossover(everything(), 1, ~ .x * .y)) %>%
    pull()

  expect_equal(df_crossover, tibble(`x_1` = 1))

  df_crossoverx <- df0 %>%
    group_by(g) %>%
    summarise(x = crossoverx(everything(), 1, ~ .x * .y)) %>%
    pull()

  expect_equal(df_crossoverx, tibble(`x_1` = 1))

})

test_that("crossover() correctly names output columns", {
  gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

  expect_named(
    mutate(gf, crossover(c(y,z), 3:4, ~ .x * .y)),
    c("x", "y", "z", "s", "y_3", "z_4")
  )
  expect_named(
    mutate(gf, crossover(c(y,z), 3:4, ~ .x * .y, .names = "id_{xcol}_{y}")),
    c("x", "y", "z", "s", "id_y_3", "id_z_4")
  )
  expect_named(
    summarise(gf, crossover(c(y,z), 3:4, ~ mean(.x + .y), .names = "mean_{xcol}_{y}")),
    c("x", "mean_y_3", "mean_z_4")
  )
  expect_named(
    summarise(gf, crossover(c(y,z), 3:4, list(paste = paste, sum = sum))),
    c("x", "y_3_paste", "y_3_sum", "z_4_paste", "z_4_sum")
  )
  expect_named(
    summarise(gf, crossover(c(y,z), 3:4, list(paste = paste, mean))),
    c("x", "y_3_paste", "y_3_2", "z_4_paste", "z_4_2")
  )
  expect_named(
    summarise(gf, crossover(c(y,z), 3:4, list(paste, mean = mean))),
    c("x", "y_3_1", "y_3_mean", "z_4_1", "z_4_mean")
  )
  expect_named(
    summarise(gf, crossover(c(y,z), 3:4, list(mean, sum))),
    c("x", "y_3_1", "y_3_2", "z_4_1", "z_4_2")
  )
  expect_named(
    summarise(gf, crossover(c(y,z),
                            3:4,
                            list(mean = mean, paste = paste),
                            .names = "{fn}_{xcol}_{y}")),
    c("x", "mean_y_3", "paste_y_3", "mean_z_4", "paste_z_4")
  )
  # further added crossover()'s y_val, y_idx, y_nm
  expect_named(
    summarise(gf, crossover(c(y,z),
                            list(a = 3, b = 4),
                            list(mean = mean, paste = paste),
                            .names = "{fn}_{xcol}_{y_val}")),
    c("x", "mean_y_3", "paste_y_3", "mean_z_4", "paste_z_4")
  )
  expect_named(
    summarise(gf, crossover(c(y,z),
                             list(a = 3, b = 4),
                             list(mean = mean, paste = paste),
                            .names = "{fn}_{xcol}_{y_nm}")),
    c("x", "mean_y_a", "paste_y_a", "mean_z_b", "paste_z_b")
  )
  expect_warning(
    summarise(gf, crossover(c(y,z),
                            list(a = 3:4, b = 5),
                            list(mean = ~ mean(.x + .y), sum = ~ sum(.x + .y)),
                            .names = "{fn}_{xcol}_{y_val}"))
  )

  expect_warning(
    summarise(gf, crossover(c(y,z),
                            list(3, 4),
                            list(mean = ~ mean(.x + .y), sum = ~ sum(.x + .y)),
                        .names = "{fn}_{y_nm}"))
  )
  expect_named(
    summarise(gf, crossover(c(y,z),
                            list(3, 4),
                            list(sum = sum))),
    c("x", "y_1_sum", "z_2_sum")
  )
  expect_named(
    summarise(gf, crossover(c(y,z),
                            list(a = 3, b = 4),
                            list(mean = mean, paste = paste),
                            .names = "{fn}_{xcol}_{y_idx}")),
    c("x", "mean_y_1", "paste_y_1", "mean_z_2", "paste_z_2")
  )
  expect_error(
    summarise(gf, crossover(c(y,z),
                            list(z = 3, y = 4),
                            list(mean = mean, paste = paste),
                            .names = "{xcol}_{y_idx}"))
  )

    # further added external vector
  col_nm_vec <- c("one", "two", "three", "four")
  expect_named(
    summarise(gf, crossover(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = col_nm_vec)),
    c("x", "one", "two", "three", "four")
  )
  # test that external vector throws error when too short
  col_nm_vec2 <- c("one", "two", "three")
  expect_error(
    summarise(gf, crossover(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = col_nm_vec2))
  )
  # test that external vector throws error when too long
  col_nm_vec3 <- c("one", "two", "three", "four", "five")
  expect_error(
    summarise(gf, crossover(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = col_nm_vec3))
  )
  expect_error(
    summarise(gf, crossover(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = "new"))
  )
  # test that external vectors throws error when it contains non-unique names
  col_nm_vec4 <- rep(c("one", "two"), 2)
  expect_error(
    summarise(gf, crossover(c(y,z),
                            list(z = 3, y = 4),
                            list(paste = paste, sum = sum),
                            .names = col_nm_vec4))
  )
})

test_that("crossoverx() correctly names output columns", {
  gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

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
  # further added crossover()'s y_val, y_idx, y_nm
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
})


test_that("crossover() result locations are aligned with .fn list names", {

  df <- tibble(x = 1:2, y = 3:4)

  expect1 <- tibble(`x_3_cls` = "integer", `x_3_type` = TRUE,
                    `y_4_cls` = "integer", `y_4_type` = TRUE)

  df_crossover <- summarise(df,
                        crossover(c(x,y), 3:4,
                              list(cls = ~ class(.x + .y),
                              type = ~ is.numeric(.x + .y))))

  expect_identical(df_crossover, expect1)

  expect2 <- tibble(`x_3_cls` = "integer", `x_3_type` = TRUE,
                    `x_4_cls` = "integer", `x_4_type` = TRUE,
                    `y_3_cls` = "integer", `y_3_type` = TRUE,
                    `y_4_cls` = "integer", `y_4_type` = TRUE)

  df_crossoverx <- summarise(df,
                 crossoverx(c(x, y), 3:4,
                       list(cls = ~ class(.x + .y),
                            type = ~ is.numeric(.x + .y))))

  expect_identical(df_crossoverx, expect2)

})


test_that("crossover() passes ... to functions", {

  df <- tibble(x = 1, y = 2)

  expect_equal(
    summarise(df, crossover(c(x, y),
                           list(a = 10, b = 20),
                           sum,
                           na.rm = TRUE)),
    tibble(x_a = 11, y_b = 22)
  )

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

  expect_equal(
    summarise(df, crossover(c(x, y),
                             list(a = 10, b = NA),
                             list(sum = sum, mean = mean2), na.rm = TRUE)),
    tibble(x_a_sum = 11, x_a_mean = 5.5,
           y_b_sum = 2, y_b_mean = 2)
  )

})

test_that("crossover() passes unnamed arguments following .fns as ...", {

  df <- tibble(x = 1, y = "b")

  expect_equal(mutate(df, crossover(c(x, y), 3:4, paste, 5)),
               tibble(x = 1, y = "b", x_3 = "1 3 5", y_4 = "b 4 5"))

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

test_that("crossover() retains original ordering", {
  df <- tibble(a = c(1:2), b = c(3:4))

  expect_equal(mutate(df, a = c(5:6), x = crossover(c(a, b), .data$b, sum))$x,
               tibble(a_3 = c(14, 14), b_4 = c(11, 11)))

  expect_equal(mutate(df, a = c(5:6), x = crossoverx(c(a, b), .data$b, sum))$x,
               tibble(a_3 = c(14, 14), a_4 = c(15, 15), b_3 = c(10, 10), b_4 = c(11, 11)))

})

test_that("crossover() gives meaningful messages", {

  # only over2, over2x specific error messages go here

  expect_snapshot_error(
    mutate(tibble(x = 1), crossover(x, c(2:3), mean))
  )

})

test_that("crossover() uses environment from the current quosure (#5460)", {
  # If the data frame `y` is selected, causes a subscript conversion
  # error since it is fractional
  df <- data.frame(x = c(1, 2), y = c(1.1, 2.4))
  y <- "x"
  expect_equal(df %>%
                 summarise(crossover(all_of(y),
                                     1,
                                     ~ mean(.x, na.rm = .y))),
               data.frame(x_1 = 1.5))

  expect_equal(df %>%
                 summarise(crossoverx(all_of(y),
                                     1,
                                     ~ mean(.x, na.rm = .y))),
               data.frame(x_1 = 1.5))

  expect_equal(df %>% filter(crossover(all_of(y), 1, ~ .x + .y <= 2)),
               slice(df, 1))

  expect_equal(df %>% filter(crossoverx(all_of(y), 1, ~ .x + .y <= 2)),
               slice(df, 1))

  # Recursive case fails because the `y` column has precedence (across issue: #5498)
  # expect_error(df %>% summarise(summarise(across(), across(all_of(y), mean))))

  # Inherited case
  out <- df %>% summarise(local(crossover(all_of(y),
                                          1,
                                          ~ mean(.x, na.rm = .y))))
  expect_equal(out, data.frame(x_1 = 1.5))

  out2 <- df %>% summarise(local(crossoverx(all_of(y),
                                          1,
                                          ~ mean(.x, na.rm = .y))))
  expect_equal(out2, data.frame(x_1 = 1.5))
})



# expected errors

test_that("crossover() custom errors", {

  # inside dplyr
  expect_error(crossover())
  expect_error(crossoverx())

  # .fns must be function
  expect_error(
    summarise(tibble(x = 1), crossover(x, 2, 42))
  )
  expect_error(
    summarise(tibble(x = 1), crossoverx(x, 2, 42))
  )

  # check keep used
  expect_error(
    mutate(tibble(x = 1), crossover(x, 2, mean), .keep = "used")
  )
  expect_error(
    mutate(tibble(x = 1), crossoverx(x, 2, mean), .keep = "used")
  )

  # check keep unused
  expect_error(
    mutate(tibble(x = 1), crossover(x, 2, mean), .keep = "unused")
  )
  expect_error(
    mutate(tibble(x = 1), crossoverx(x, 2, mean), .keep = "unused")
  )

  # no existing colnames
  expect_error(
    mutate(iris, crossover(Sepal.Length, 2, paste, .names = "{xcol}"))
  )
  expect_error(
    mutate(iris, crossoverx("Sepal.Length", c(2,3), paste,
                        .names = c("Sepal.Length", "Sepal.Width")))
  )

  # over2 specific errors
  expect_error(
    mutate(tibble(x = 1), crossover(x, c(2:3), mean))
  )

})

# other edge cases
