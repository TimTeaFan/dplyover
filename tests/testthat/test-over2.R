# over2 ------------------------------------------------------------------
library(dplyr)

# over examples of basic functionality from the example section
test_that("over2() exmample", {

  brks <- list(b1 = 3:8,
               b2 = seq(3, 9, by = 2))

  labs <- list(l1 = c("3 to 4", "4 to 5", "5 to 6",
                      "6 to 7", "7 to 8"),
               l2 = c("3 to 5", "5 to 7", "7 to 9"))

  df_over2 <- iris %>%
    transmute(over2(brks, labs,
                    ~ cut(Sepal.Length,
                          breaks = .x,
                          labels = .y),
                    .names = "Sepal.Length.cut{x_idx}"))

  df_expect <- iris %>%
    transmute(Sepal.Length.cut1 = cut(Sepal.Length,
                                      breaks = brks[[1]],
                                      labels = labs[[1]]),
              Sepal.Length.cut2 = cut(Sepal.Length,
                                      breaks = brks[[2]],
                                      labels = labs[[2]]))

  expect_equal(df_over2, df_expect)

})

test_that("over2x() exmample", {

  df_over2x <- csat %>%
    transmute(over2x(unique(type),
                     unique(product),
                     ~ type == .x & product == .y))

  df_expect <- csat %>%
    transmute(existing_advanced = type == "existing" & product == "advanced",
              existing_premium = type == "existing" & product == "premium",
              existing_basic = type == "existing" & product == "basic",
              reactivate_advanced = type == "reactivate" & product == "advanced",
              reactivate_premium = type == "reactivate" & product == "premium",
              reactivate_basic = type == "reactivate" & product == "basic",
              new_advanced = type == "new" & product == "advanced",
              new_premium = type == "new" & product == "premium",
              new_basic = type == "new" & product == "basic")

  expect_equal(df_over2x, df_expect)

})

# tests adopted from across
test_that("over2() works on one column data.frame", {

  df0 <- data.frame(x = 1)

  df_over2 <- df0 %>%
    mutate(over2(1, 1, ~ x * .x * .y))

  df_exepect <- df0 %>%
    mutate(`1_1` = x * 1)

  expect_equal(df_over2, df_exepect)

  df_over2x <- df0 %>%
    mutate(over2(1, 1, ~ x * .x * .y))

  expect_equal(df_over2x, df_exepect)

})

test_that("over() does not select grouping variables", {

  df0 <- data.frame(g = 1, x = 1)

  df_over2 <- df0 %>%
    group_by(g) %>%
    summarise(x = over2(1, 1, ~ x * .x * .y)) %>%
    pull()

  expect_equal(df_over2, tibble(`1_1` = 1))

  df_over2x <- df0 %>%
    group_by(g) %>%
    summarise(x = over2x(1, 1, ~ x * .x * .y)) %>%
    pull()

  expect_equal(df_over2x, tibble(`1_1` = 1))

})

test_that("over2() correctly names output columns", {
  gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

  expect_named(
    mutate(gf, over2(1:2, 3:4, ~ x * .x * .y)),
    c("x", "y", "z", "s", "1_3", "2_4")
  )
  expect_named(
    mutate(gf, over2(1:2, 3:4, ~ x * .x * .y, .names = "id_{x}_{y}")),
    c("x", "y", "z", "s", "id_1_3", "id_2_4")
  )
  expect_named(
    summarise(gf, over2(1:2, 3:4, ~ mean(x + .x + .y), .names = "mean_{x}_{y}")),
    c("x", "mean_1_3", "mean_2_4")
  )
  expect_named(
    summarise(gf, over2(1:2, 3:4, list(mean = mean, sum = sum))),
    c("x", "1_3_mean", "1_3_sum", "2_4_mean", "2_4_sum")
  )
  expect_named(
    summarise(gf, over2(1:2, 3:4, list(mean = mean, sum))),
    c("x", "1_3_mean", "1_3_2", "2_4_mean", "2_4_2")
  )
  expect_named(
    summarise(gf, over2(1:2, 3:4, list(mean, sum = sum))),
    c("x", "1_3_1", "1_3_sum", "2_4_1", "2_4_sum")
  )
  expect_named(
    summarise(gf, over2(1:2, 3:4, list(mean, sum))),
    c("x", "1_3_1", "1_3_2", "2_4_1", "2_4_2")
  )
  expect_named(
    summarise(gf, over2(1:2, 3:4, list(mean = mean, sum = sum), .names = "{fn}_{x}_{y}")),
    c("x", "mean_1_3", "sum_1_3", "mean_2_4", "sum_2_4")
  )
  # further added over()'s x_val, x_idx, x_nm
  expect_named(
    summarise(gf, over2(list(a = 5, b = 6, c = 7),
                        list(x = 1, y = 2, z = 3),
                        list(mean = mean, sum = sum),
                        .names = "{fn}_{x_val}_{y_val}")),
    c("x", "mean_5_1", "sum_5_1",  "mean_6_2", "sum_6_2", "mean_7_3", "sum_7_3")
  )
  expect_named(
    summarise(gf, over2(list(a = 5, b = 6, c = 7),
                        list(x = 1, y = 2, z = 3),
                        list(mean = mean, sum = sum),
                        .names = "{fn}_{x_nm}_{y_nm}")),
    c("x", "mean_a_x", "sum_a_x",  "mean_b_y", "sum_b_y", "mean_c_z", "sum_c_z")
  )
  expect_warning(
    summarise(gf, over2(list(a = 5:6, b = 6, c = 7),
                        list(x = 1, y = 2, z = 3),
                        list(mean = ~ mean(.x + .y), sum = ~ sum(.x + .y)),
                       .names = "{fn}_{x_val}_{y_val}"))
  )
  expect_warning(
    summarise(gf, over2(list(a = 5, b = 6, c = 7),
                        list(x = 1, y = 2:3, z = 3),
                        list(mean = ~ mean(.x + .y), sum = ~ sum(.x + .y)),
                        .names = "{fn}_{x_val}_{y_val}"))
  )
  expect_named(
    summarise(gf, over2(list(a = 5, b = 6, c = 7),
                        list(x = 1, y = 2, z = 3),
                        list(mean = mean, sum = sum),
                       .names = "{fn}_{x_nm}{y_nm}")),
    c("x", "mean_ax", "sum_ax",  "mean_by", "sum_by", "mean_cz", "sum_cz")
  )
  expect_warning(
    summarise(gf,  over2(list(5, 6, 7),
                         list(1, 2, 3),
                         list(mean = mean, sum = sum),
                        .names = "{fn}_{x_nm}"))
  )
  expect_named(
    summarise(gf, over2(list(a = 5, b = 6, c = 7),
                        list(x = 1, y = 2, z = 3),
                        list(mean = mean, sum = sum),
                        .names = "{fn}_{x_idx}_{y_idx}")),
    c("x", "mean_1_1", "sum_1_1",  "mean_2_2", "sum_2_2", "mean_3_3", "sum_3_3")
  )
  expect_error(
    summarise(gf, over2(list(a = 5, b = 5, c = 7),
                        list(x = 1, y = 1, z = 3),
                        list(mean = mean, sum = sum),
                       .names = "{fn}_{x_val}_{y_val}"))
  )
  expect_error(
    summarise(gf, over2(list(a = 5, a = 6, c = 7),
                        list(x = 1, x = 1, z = 3),
                        list(mean = mean, sum = sum),
                        .names = "{fn}_{x_nm}_{y_nm}"))
  )
  expect_error(
    summarise(gf, over2(list(a = 5, a = 6, c = 7),
                        list(x = 1, x = 1, z = 3),
                        list(mean = mean, sum = sum)))
  )
  # further added external vector
  col_nm_vec <- c("one", "two", "three", "four", "five", "six")
  expect_named(
    summarise(gf, over2(list(a = 5, b = 6, c = 7),
                        list(x = 1, x = 1, z = 3),
                        list(mean = mean, sum = sum),
                        .names = col_nm_vec)),
    c("x", "one", "two", "three", "four", "five", "six")
  )
  # test that external vector throws error when too short
  col_nm_vec2 <- c("one", "two", "three")
  expect_error(
    summarise(gf, over2(list(a = 5, b = 6, c = 7),
                        list(x = 1, x = 1, z = 3),
                        list(mean = mean, sum = sum),
                        .names = col_nm_vec2))
  )
  # test that external vector throws error when too long
  col_nm_vec3 <- c("one", "two", "three", "four", "five", "six", "seven")
  expect_error(
    summarise(gf, over2(list(a = 5, b = 6, c = 7),
                        list(x = 1, x = 1, z = 3),
                        list(mean = mean, sum = sum),
                        .names = col_nm_vec3))
  )
  # test that external vectors throws error when it contains non-unique names
  col_nm_vec4 <- rep(col_nm_vec2 <- c("one", "two", "three"), 2)
  expect_error(
    summarise(gf, over2(list(a = 5, b = 6, c = 7),
                        list(x = 1, x = 1, z = 3),
                        list(mean = mean, sum = sum),
                        .names = col_nm_vec4))
  )
})

test_that("over2x() correctly names output columns", {
  gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

  expect_named(
    mutate(gf, over2x(1:2, 3:4, ~ x * .x * .y)),
    c("x", "y", "z", "s", "1_3", "1_4", "2_3", "2_4")
  )
  expect_named(
    mutate(gf, over2x(1:2, 3:4, ~ x * .x * .y, .names = "id_{x}_{y}")),
    c("x", "y", "z", "s", "id_1_3", "id_1_4", "id_2_3", "id_2_4")
  )
  expect_named(
    summarise(gf, over2x(1:2, 3:4, ~ mean(x + .x + .y), .names = "mean_{x}_{y}")),
    c("x", "mean_1_3", "mean_1_4", "mean_2_3", "mean_2_4")
  )
  expect_named(
    summarise(gf, over2x(1:2, 3:4, list(mean = ~ mean(c(.x,.y)), sum = sum))),
    c("x", "1_3_mean", "1_3_sum", "1_4_mean", "1_4_sum", "2_3_mean", "2_3_sum", "2_4_mean", "2_4_sum")
  )
  expect_named(
    summarise(gf, over2x(1:2, 3:4, list(mean = mean, sum))),
    c("x", "1_3_mean", "1_3_2", "1_4_mean", "1_4_2", "2_3_mean", "2_3_2", "2_4_mean", "2_4_2")
  )
  expect_named(
    summarise(gf, over2x(1:2, 3:4, list(mean, sum = sum))),
    c("x", "1_3_1", "1_3_sum", "1_4_1", "1_4_sum", "2_3_1", "2_3_sum", "2_4_1", "2_4_sum")
  )
  expect_named(
    summarise(gf, over2x(1:2, 3:4, list(mean, sum))),
    c("x", "1_3_1", "1_3_2", "1_4_1", "1_4_2", "2_3_1", "2_3_2", "2_4_1", "2_4_2")
  )
  expect_named(
    summarise(gf, over2x(1:2, 3:4, list(mean = mean, sum = sum), .names = "{fn}_{x}_{y}")),
    c("x", "mean_1_3", "sum_1_3", "mean_1_4", "sum_1_4", "mean_2_3", "sum_2_3", "mean_2_4", "sum_2_4")
  )
  # further added over()'s x_val, x_idx, x_nm
  expect_named(
    summarise(gf, over2x(list(a = 5, b = 6),
                         list(x = 1, y = 2),
                         list(mean = mean, sum = sum),
                         .names = "{fn}_{x_val}_{y_val}")),
    c("x", "mean_5_1", "sum_5_1",  "mean_5_2", "sum_5_2", "mean_6_1", "sum_6_1", "mean_6_2", "sum_6_2")
  )
  expect_named(
    summarise(gf, over2x(list(a = 5, b = 6),
                         list(x = 1, y = 2),
                         list(mean = mean, sum = sum),
                         .names = "{fn}_{x_nm}_{y_nm}")),
    c("x", "mean_a_x", "sum_a_x",  "mean_a_y", "sum_a_y", "mean_b_x", "sum_b_x", "mean_b_y", "sum_b_y")
  )
  expect_warning(
    summarise(gf, over2x(list(a = 5:6, b = 6),
                         list(x = 1, y = 2),
                         list(mean = ~ mean(.x + .y), sum = ~ sum(.x + .y)),
                         .names = "{fn}_{x_val}_{y_val}"))
  )
  expect_warning(
    summarise(gf, over2x(list(a = 5, b = 6),
                         list(x = 1, y = 2:3),
                         list(mean = ~ mean(.x + .y), sum = ~ sum(.x + .y)),
                         .names = "{fn}_{x_val}_{y_val}"))
  )
  expect_named(
    summarise(gf, over2x(list(a = 5, b = 6),
                         list(x = 1, y = 2),
                         list(mean = mean, sum = sum),
                         .names = "{fn}_{x_nm}{y_nm}")),
    c("x", "mean_ax", "sum_ax",  "mean_ay", "sum_ay", "mean_bx", "sum_bx", "mean_by", "sum_by")
  )
  expect_warning(expect_warning(
    summarise(gf, over2x(list(5, 6),
                         list(1, 2),
                         list(mean = mean, sum = sum),
                         .names = "{fn}_{x_nm}_{y_nm}"))
  ))
  expect_named(
    summarise(gf, over2x(list(a = 5, b = 6),
                         list(x = 1, y = 2),
                         list(mean = mean, sum = sum),
                         .names = "{fn}_{x_idx}_{y_idx}")),
    c("x", "mean_1_1", "sum_1_1",  "mean_1_2", "sum_1_2", "mean_2_1", "sum_2_1", "mean_2_2", "sum_2_2")
  )
  expect_error(
    summarise(gf, over2x(list(a = 5, b = 5),
                         list(x = 1, y = 1),
                         list(mean = mean, sum = sum),
                         .names = "{fn}_{x_val}_{y_val}"))
  )
  expect_error(
    summarise(gf, over2x(list(a = 5, a = 6),
                         list(x = 1, x = 1),
                         list(mean = mean, sum = sum),
                         .names = "{fn}_{x_nm}_{y_nm}"))
  )
  expect_error(
    summarise(gf, over2x(list(a = 5, a = 6),
                         list(x = 1, x = 1),
                         list(mean = mean, sum = sum)))
  )
  # further added external vector
  col_nm_vec <- c("one", "two", "three", "four", "five", "six", "seven", "eight")
  expect_named(
    summarise(gf, over2x(list(a = 5, b = 6),
                         list(x = 1, y = 2),
                         list(mean = mean, sum = sum),
                         .names = col_nm_vec)),
    c("x", "one", "two", "three", "four", "five", "six", "seven", "eight")
  )
  # test that external vector throws error when too short
  col_nm_vec2 <- c("one", "two", "three")
  expect_error(
    summarise(gf, over2x(list(a = 5, b = 6),
                         list(x = 1, y = 2),
                         list(mean = mean, sum = sum),
                         .names = col_nm_vec2))
  )
  # test that external vector throws error when too long
  col_nm_vec3 <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  expect_error(
    summarise(gf, over2x(list(a = 5, b = 6),
                         list(x = 1, y = 2),
                         list(mean = mean, sum = sum),
                         .names = col_nm_vec3))
  )
  # test that external vectors throws error when it contains non-unique names
  col_nm_vec4 <- rep(col_nm_vec2 <- c("one", "two", "three", "four"), 2)
  expect_error(
    summarise(gf, over2x(list(a = 5, b = 6),
                         list(x = 1, y = 2),
                         list(mean = mean, sum = sum),
                         .names = col_nm_vec4))
  )
})


test_that("over2x() result locations are aligned with .fn list names", {

  df <- tibble(x = 1:2)

  expect1 <- tibble(`1_3_cls` = "integer", `1_3_type` = TRUE,
                    `2_4_cls` = "integer", `2_4_type` = TRUE)

  df_over2 <- summarise(df,
                        over2x(1:2, 3:4,
                              list(cls = ~ class(x + .x),
                              type = ~ is.numeric(x + .x))))

  expect_identical(df_over2, expect1)

  expect2 <- tibble(`1_3_cls` = "integer", `1_3_type` = TRUE,
                    `1_4_cls` = "integer", `1_4_type` = TRUE,
                    `2_3_cls` = "integer", `2_3_type` = TRUE,
                    `2_4_cls` = "integer", `2_4_type` = TRUE)

  df_over2x <- summarise(df,
                 over2x(1:2, 3:4,
                       list(cls = ~ class(x + .x + .y),
                            type = ~ is.numeric(x + .x + .y))))

  expect_identical(df_over2x, expect2)

})

# up to here
test_that("over2() passes ... to functions", {

  df <- tibble(x = 1)

  expect_equal(
    summarise(df, over(list(a = c(1,NA)), mean, na.rm = TRUE)),
    tibble(a = 1)
  )

  expect_equal(
    summarise(df, over(list(a = c(1,NA)), list(mean = mean, median = median), na.rm = TRUE)),
    tibble(a_mean = 1, a_median = 1)
  )
})

test_that("over() passes unnamed arguments following .fns as ...", {

  df <- tibble(x = 1)

  expect_equal(mutate(df, over(2, `+`, 1)),
               tibble(x = 1, `2` = 3))
})

test_that("over() avoids simple argument name collisions with ... ", {

  df <- tibble(x = c(1, 2))

  expect_equal(summarize(df, over(list(a = c(1:10)), tail, n = 1)),
               tibble(`a` = 10))
})

test_that("over() works sequentially", {

  df <- tibble(a = 1)

  expect_equal(
    mutate(df,
           x = ncol(over(1, mean)),
           y = ncol(over(1:2, mean))),
    tibble(a = 1, x = 1L, y = 2L)
  )

  expect_equal(
    mutate(df,
           a = "x",
           y = ncol(over(1, mean))),
    tibble(a = "x", y = 1L)
  )

  expect_equal(
    mutate(df,
           x = 1,
           y = ncol(over(1:2, mean))),
    tibble(a = 1, x = 1, y = 2L)
  )
})

test_that("across() retains original ordering", {
  df <- tibble(a = 1, b = 2)
  expect_named(mutate(df, a = 2, x = across())$x, c("a", "b"))
})

test_that("over() gives meaningful messages", {

  # inside dplyr
  expect_snapshot_error(over())

  # mutate .keep = "used"
  expect_snapshot_error(
    mutate(tibble(x = 1),
           over(1, mean),
           .keep = "used")
    )

  # mutate .keep = "unused"
  expect_snapshot_error(
    mutate(tibble(x = 1),
           over(1, mean),
           .keep = "unused")
  )

  # .fns must be function
  expect_snapshot_error(
    summarise(tibble(x = 1), over(1, 42))
  )

  # check keep used
  expect_snapshot_error(
    mutate(tibble(x = 1),
           over(1, mean),
           .keep = "used")
  )

  # check keep unused
  expect_snapshot_error(
    mutate(tibble(x = 1),
           over(1, mean),
           .keep = "unused")
  )

  # no existing colnames
  expect_snapshot_error(
    mutate(iris, over("Sepal.Length", paste))
  )

  # vector to .names too short
  expect_snapshot_error({
    gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)
    col_nm_vec2 <- c("one", "two", "three")
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = col_nm_vec2))
  })

  # vector to .names too long
  expect_snapshot_error({
    gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)
    col_nm_vec3 <- c("one", "two", "three", "four", "five", "six", "seven")
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = col_nm_vec3))
  })

  # vector to .names duplicate names
  expect_snapshot_error({
    gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)
    col_nm_vec4 <- rep(col_nm_vec2 <- c("one", "two", "three"), 2)
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = col_nm_vec4))
  })

})

test_that("monitoring cache - over() can be used twice in the same expression", {
  df <- tibble(a = 1, b = 2)
  expect_equal(
    mutate(df, x = ncol(over(1, mean) + ncol(over(1, mean)))),
    tibble(a = 1, b = 2, x = 1)
    )
})

test_that("monitoring cache - over() can be used in separate expressions", {
  df <- tibble(a = 1, b = 2)
  expect_equal(
    mutate(df,
           x = ncol(over(1:2, mean)),
           y = ncol(over(1, mean))),
    tibble(a = 1, b = 2, x = 2, y = 1)
  )
})

test_that("monitoring cache - over() usage can depend on the group id", {
  df <- tibble(g = 1:2, a = 1:2, b = 3:4)
  df <- group_by(df, g)

  switcher <- function() {
    if_else(cur_group_id() == 1L, over(1L:2L, mean)$`1`, over(3L:4L, mean)$`4`)
  }

  expect <- df
  expect$x <- c(1L, 4L)

  expect_equal(
    mutate(df, x = switcher()),
    expect
  )
})

test_that("monitoring cache - over() internal cache key depends on all inputs", {
  df <- tibble(g = rep(1:2, each = 2), a = 1:4)
  df <- group_by(df, g)

  expect_identical(
    mutate(df,
           tibble(x = over(1, ~ mean(.x + a))$`1`,
                  y = over(list(b = 1:2), ~ max(.x + a))$b)),
    mutate(df, x = mean(a + 1), y = max(a + 1:2))
  )
})

test_that("over() rejects non vectors", {
  expect_error(
    data.frame(x = 1) %>% summarise(over(1, ~sym("foo")))
  )
})

test_that("over() uses tidy recycling rules", {
  expect_equal(
    tibble::tibble(x = 1, y = 2) %>% summarise(over(1:2, ~ rep(42, .))),
    tibble::tibble(`1` = rep(42, 2), `2` = rep(42, 2))
  )

  expect_error(
    data.frame(x = 2, y = 3) %>% summarise(over(1:3, ~rep(42, .)))
  )
})

test_that("over(<empty set>, foo) returns a data frame with 1 row", {
  df <- tibble(x = 1:42)
  expect_equal(
    mutate(df, over(c(), mean)),
    df
  )
  expect_equal(
    mutate(df, y = over(c(), mean))$y,
    tibble::new_tibble(list(), nrow = 42)
  )
  mutate(df, {
    res <- over(c(), mean)
    expect_equal(nrow(res), 1L)
    res
  })
})


# expected errors

test_that("over() custom errors", {

  # inside dplyr
  expect_error(over())

  # .fns must be function
    expect_error(
    summarise(tibble(x = 1), over(1, 42))
  )

  # check keep used
    expect_error(
    mutate(tibble(x = 1),
           over(1, mean),
           .keep = "used")
  )

  # check keep unused
    expect_error(
    mutate(tibble(x = 1),
           over(1, mean),
           .keep = "unused")
  )

  # no existing colnames
    expect_error(
    mutate(iris, over("Sepal.Length", paste))
  )

})

# other edge cases
