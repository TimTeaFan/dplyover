# across2 ------------------------------------------------------------------
library(dplyr)

# across2 examples of basic functionality from the example section

test_that("across2() example", {

  df_across2 <- iris %>%
    transmute(across2(ends_with("Length"),
                      ends_with("Width"),
                      .fns = list(product = ~ .x * .y,
                                  sum = ~ .x + .y),
                      .names = "{pre}_{fn}",
                      .names_fn = tolower))

  df_expect <- iris %>%
    transmute(sepal_product = Sepal.Length * Sepal.Width,
              sepal_sum = Sepal.Length + Sepal.Width,
              petal_product = Petal.Length * Petal.Width,
              petal_sum = Petal.Length + Petal.Width)

  expect_equal(df_across2, df_expect)
})


test_that("across2x() example", {

  df_across2x <- iris %>%
    group_by(Species) %>%
    summarise(across2x(everything(),
                       everything(),
                       ~ round(cor(.x, .y), 2),
                       .names_fn = ~ gsub("Sepal", "S", .x) %>%
                         gsub("Petal", "P", .),
                       .comb = "minimal"))

  df_expect <- iris %>%
    group_by(Species) %>%
    summarise(S.Length_S.Width = round(cor(Sepal.Length, Sepal.Width), 2),
              S.Length_P.Length = round(cor(Sepal.Length, Petal.Length), 2),
              S.Length_P.Width = round(cor(Sepal.Length, Petal.Width), 2),
              S.Width_P.Length = round(cor(Sepal.Width, Petal.Length), 2),
              S.Width_P.Width = round(cor(Sepal.Width, Petal.Width), 2),
              P.Length_P.Width = round(cor(Petal.Length, Petal.Width), 2))

  expect_equal(df_across2x, df_expect)
})


# tests adopted from across

test_that("across2() works on one column data.frame", {
  df <- data.frame(x = 1)
  df_across2 <- df %>% mutate(across2(x, x, sum))
  df_expect <- data.frame(x = 1, x_x = 2)
  expect_equal(df_across2, df_expect)

  df_across2x <- df %>% mutate(across2x(x, x, sum))
  df_expect <- data.frame(x = 1, x_x = 2)
  expect_equal(df_across2x, df_expect)

  # df <- data.frame(x = 1, y = 2)
  # df_across2x <- df %>% mutate(across2x(x, c(x, y), sum, .names = "X{idx}"))
  # df_expect <- data.frame(x = 1, y = 2, `X1` = 2, `X2` = 3)
  # expect_equal(df_across2x, df_expect)

})

test_that("across2() does not select grouping variables", {
  df <- data.frame(g = 1, x = 1) %>% group_by(g)

  df_across2 <- df %>%
    summarise(x = across2(everything(),
                          everything(), ~ .x)) %>% pull()
  expect_equal(df_across2, tibble(x_x = 1))

  df_across2x <- df %>%
    summarise(x = across2(everything(),
                          everything(), ~ .x)) %>% pull()
  expect_equal(df_across2x, tibble(x_x = 1))

})

test_that("across2() correctly names output columns", {
  gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

  expect_named(
    summarise(gf, across2(everything(), everything(), ~ .x)),
    c("x", "y_y", "z_z", "s_s")
  )
  expect_named(
    summarise(gf, across2(everything(), everything(), ~ .x,
                          .names = "id_{xcol}{ycol}")),
    c("x", "id_yy", "id_zz", "id_ss")
  )
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric), sum)),
    c("x", "y_y", "z_z")
  )
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric), sum, .names = "sum_{xcol}{ycol}")),
    c("x", "sum_yy", "sum_zz")
  )
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric),
                          list(paste = paste0, sum = sum))),
    c("x", "y_y_paste", "y_y_sum", "z_z_paste", "z_z_sum")
  )
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric),
                          list(paste = paste0, sum))),
    c("x", "y_y_paste", "y_y_2", "z_z_paste", "z_z_2")
  )
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric),
                          list(paste, sum = sum))),
    c("x", "y_y_1", "y_y_sum", "z_z_1", "z_z_sum")
  )
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric), list(paste0, sum))),
    c("x", "y_y_1", "y_y_2", "z_z_1", "z_z_2")
  )
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric),
                          list(mean = mean, sum = sum), .names = "{fn}_{xcol}")),
    c("x", "mean_y", "sum_y", "mean_z", "sum_z")
  )

  # pre
  expect_named(
    summarise(iris, across2(ends_with("Length"), ends_with("Width"),
                          list(product = ~ .x * .y, sum = sum),
                          .names = "{pre}_{fn}",
                          .names_fn = tolower)),
    c("sepal_product", "sepal_sum", "petal_product", "petal_sum")
  )
  expect_error(
    summarise(iris, across2(starts_with("Sepal"), starts_with("Petal"),
                            list(product = ~ .x * .y, sum = sum),
                            .names = "{pre}_{fn}",
                            .names_fn = tolower))
  )
  # suf
  expect_named(
    summarise(iris, across2(starts_with("Sepal"), starts_with("Petal"),
                            list(product = ~ .x * .y, sum = sum),
                            .names = "{suf}_{fn}",
                            .names_fn = tolower)),
    c("length_product", "length_sum", "width_product", "width_sum")
  )
  expect_error(
    summarise(iris, across2(c(Sepal.Length, Species), c(Petal.Length, Sepal.Length),
                            list(product = ~ .x * .y, sum = sum),
                            .names = "{suf}_{fn}",
                            .names_fn = tolower))
  )
  # idx
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric),
                          list(mean = mean, sum = sum), .names = "new_col{idx}")),
    c("x", "new_col1", "new_col2", "new_col3", "new_col4")
  )

  # external vector (and errors)
  col_nm_vec <- c("one", "two", "three", "four")
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric),
                          list(mean = mean, sum = sum),
                          .names = col_nm_vec)),
    c("x", col_nm_vec)
  )
  col_nm_vec2 <- c("one", "two", "three")
  expect_error(
    summarise(gf, across2(where(is.numeric), where(is.numeric),
                          list(mean = mean, sum = sum),
                          .names = col_nm_vec2))
  )
  col_nm_vec3 <- c("one", "two", "three", "four", "five")
  expect_error(
    summarise(gf, across2(where(is.numeric), where(is.numeric),
                          list(mean = mean, sum = sum),
                          .names = col_nm_vec3))
  )
  expect_error(
    summarise(gf, across2(c(y,z), c(z,s),
                          list(paste = paste),
                          .names = "newcol"))
  )
})


test_that("across2x() correctly names output columns", {
  gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

  expect_named(
    summarise(gf, across2x(y, c(y,z), ~ .x)),
    c("x", "y_y", "y_z")
  )
  expect_named(
    summarise(gf, across2x(y, c(y,z), ~ .x,
                           .names = "id_{xcol}{ycol}")),
    c("x", "id_yy", "id_yz")
  )
  expect_named(
    summarise(gf, across2x(where(is.numeric), where(is.numeric), sum)),
    c("x", "y_y", "y_z", "z_y", "z_z")
  )
  expect_named(
    summarise(gf, across2x(where(is.numeric),
                           where(is.numeric),
                           sum,
                           .names = "sum_{xcol}{ycol}")),
    c("x", "sum_yy", "sum_yz", "sum_zy", "sum_zz")
  )
  expect_named(
    summarise(gf, across2x(where(is.numeric),
                           where(is.numeric),
                           list(paste = paste0, sum = sum))),
    c("x", "y_y_paste", "y_y_sum", "y_z_paste", "y_z_sum", "z_y_paste", "z_y_sum", "z_z_paste", "z_z_sum")
  )
  expect_named(
    summarise(gf, across2x(where(is.numeric),
                           where(is.numeric),
                           list(paste = paste0, sum))),
    c("x",  "y_y_paste", "y_y_2", "y_z_paste", "y_z_2", "z_y_paste", "z_y_2", "z_z_paste", "z_z_2")
  )
  expect_named(
    summarise(gf, across2(where(is.numeric), where(is.numeric),
                          list(paste, sum = sum))),
    c("x", "y_y_1", "y_y_sum", "z_z_1", "z_z_sum")
  )
  expect_named(
    summarise(gf, across2x(where(is.numeric), where(is.numeric), list(paste0, sum))),
    c("x", "y_y_1", "y_y_2", "y_z_1", "y_z_2", "z_y_1", "z_y_2",  "z_z_1", "z_z_2")
  )
  expect_named(
    summarise(gf, across2x(where(is.numeric), where(is.numeric),
                           list(mean = mean, sum = sum), .names = "{fn}_{xcol}{ycol}")),
    c("x", "mean_yy", "sum_yy", "mean_yz", "sum_yz", "mean_zy", "sum_zy", "mean_zz", "sum_zz")
  )

  # idx
  expect_named(
    summarise(gf, across2x(where(is.numeric),
                           where(is.numeric),
                           list(mean = mean, sum = sum),
                           .names = "new_col{idx}")),
    c("x", paste0("new_col", 1:8))
  )

  # external vector (and errors)
  col_nm_vec <- c("one", "two", "three", "four", "five", "six", "seven", "eight")
  expect_named(
    summarise(gf, across2x(where(is.numeric), where(is.numeric),
                           list(mean = mean, sum = sum),
                           .names = col_nm_vec)),
    c("x", col_nm_vec)
  )
  col_nm_vec2 <-  c("one", "two", "three", "four")
  expect_error(
    summarise(gf, across2x(where(is.numeric), where(is.numeric),
                           list(mean = mean, sum = sum),
                           .names = col_nm_vec2))
  )
  col_nm_vec3 <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  expect_error(
    summarise(gf, across2x(where(is.numeric), where(is.numeric),
                          list(mean = mean, sum = sum),
                          .names = col_nm_vec3))
  )
  expect_error(
    summarise(gf, across2x(c(y,z), c(z,s),
                           list(paste = paste),
                           .names = "newcol"))
  )
})

test_that("across2() result locations are aligned with column names (#4967)", {
  df1 <- tibble(x = 1:2, y = 3:4, z = c("a", "b"))

  df_across2 <- summarise(df1, across2(c(x, y), c(y, z),
                                      list(cls = ~ class(c(.x, .y)) ,
                                           type = ~ is.numeric(c(.x, .y)) )))

  expect1 <- tibble(x_y_cls = "integer", x_y_type = TRUE,
                   y_z_cls = "character", y_z_type = FALSE)

  expect_identical(df_across2, expect1)

  df_across2x <- summarise(df1, across2x(c(x, y), c(y, z),
                                      list(cls = ~ class(c(.x, .y)) ,
                                           type = ~ is.numeric(c(.x, .y)) )))

  expect2 <- tibble(x_y_cls = "integer", x_y_type = TRUE,
                   x_z_cls = "character", x_z_type = FALSE,
                   y_y_cls = "integer", y_y_type = TRUE,
                   y_z_cls = "character", y_z_type = FALSE)

  expect_identical(df_across2x, expect2)

})

test_that("across2() passes ... to functions", {
  df <- tibble(x = c(1, NA))
  expect_equal(
    summarise(df, across2(everything(), everything(), sum, na.rm = TRUE)),
    tibble(x_x = 2)
  )
  expect_equal(
    summarise(df, across2(everything(),
                          everything(),
                          list(paste = paste), "test")),
    tibble(x_x_paste = c("1 1 test", "NA NA test"))
  )

  expect_equal(
    summarise(df, across2x(everything(), everything(), sum, na.rm = TRUE)),
    tibble(x_x = 2)
  )
  expect_equal(
    summarise(df, across2x(everything(),
                           everything(),
                           list(paste = paste), "test")),
    tibble(x_x_paste = c("1 1 test", "NA NA test"))
  )
})

test_that("across2() passes unnamed arguments following .fns as ... (#4965)", {
  df <- tibble(x = 1)
  expect_equal(mutate(df, across2(x, x, sum, 1)), tibble(x = 1, x_x = 3))
  expect_equal(mutate(df, across2x(x, x, sum, 1)), tibble(x = 1, x_x = 3))
})

test_that("across2() avoids simple argument name collisions with ... (#4965)", {
  df <- tibble(x = c(1, 2))
  expect_equal(summarize(df, across2(x, x, tail, n = 1)), tibble(x_x = 2))
  expect_equal(summarize(df, across2x(x, x, tail, n = 1)), tibble(x_x = 2))
})

test_that("across2() works sequentially (#4907)", {
  df <- tibble(a = 1)
  expect_equal(
    mutate(df,
           x = ncol(across2(where(is.numeric), where(is.numeric), ~ .x + .y, .names = "{xcol}")),
           y = ncol(across2(where(is.numeric), where(is.numeric), ~ .x + .y, .names = "{xcol}"))),
    tibble(a = 1, x = 1L, y = 2L)
  )
  expect_equal(
    mutate(df, a = "x", y = ncol(across2(where(is.numeric), where(is.numeric), ~ .x + .y, .names = "{xcol}"))),
    tibble(a = "x", y = 0L)
  )
  expect_equal(
    mutate(df, x = 1, y = ncol(across2(where(is.numeric), where(is.numeric), ~ .x + .y, .names = "{xcol}"))),
    tibble(a = 1, x = 1, y = 2L)
  )

  expect_equal(
    mutate(df,
           x = ncol(across2x(where(is.numeric), where(is.numeric), ~ .x + .y, .names = "{xcol}")),
           y = ncol(across2x(where(is.numeric), where(is.numeric), ~ .x + .y, .names = "{xcol}{ycol}"))),
    tibble(a = 1, x = 1L, y = 4L)
  )
  expect_equal(
    mutate(df, a = "x", y = ncol(across2x(where(is.numeric), where(is.numeric), ~ .x + .y, .names = "{xcol}"))),
    tibble(a = "x", y = 0L)
  )
  expect_equal(
    mutate(df, x = 1, y = ncol(across2x(where(is.numeric), where(is.numeric), ~ .x + .y, .names = "{xcol}{ycol}"))),
    tibble(a = 1, x = 1, y = 4L)
  )

})

test_that("across2() retains original ordering", {
  df <- tibble(a = 1, b = 2)
  expect_equal(mutate(df, a = 2, x = across2(a, b, ~ .x + .y))$x,
               tibble(a_b = 4))
  expect_equal(mutate(df, a = 2, x = across2x(a, c(a,b), ~ .x + .y))$x,
               tibble(a_a = 4, a_b = 4))
})

# meaningful error messages

# snapshot tests for across2 and across2x specific errors come here







# FAILING
# test_that("monitoring cache - across2() can be used twice in the same expression", {
#   df <- tibble(a = 1, b = 2)
#   expect_equal(
#     mutate(df,
#            x = ncol(across2(a, b, ~ .x + .y)) + ncol(across2(where(is.numeric), where(is.numeric), ~ .x + .y))),
#     tibble(a = 1, b = 2, x = 4L)
#   )
# })

test_that("monitoring cache - across2() can be used in separate expressions", {
  df <- tibble(a = 1, b = 2)
  expect_equal(
    mutate(df,
           x = ncol(across2(a, b, ~ .x + .y)),
           y = ncol(across2(where(is.numeric), where(is.numeric), ~ .x + .y))),
    tibble(a = 1, b = 2, x = 1L, y = 3L)
  )
})

test_that("monitoring cache - across2() usage can depend on the group id", {
  df <- tibble(g = 1:2, a = 1:2, b = 3:4)
  df <- group_by(df, g)

  switcher <- function() {
    if_else(cur_group_id() == 1L,
            across2(a, a, ~ .x + .y)$a_a,
            across2(a, b, ~ .x + .y)$a_b)
  }

  expect <- df
  expect$x <- c(2L, 6L)

  expect_equal(
    mutate(df, x = switcher()),
    expect
  )
})

test_that("monitoring cache - across2() internal cache key depends on all inputs", {
  df <- tibble(g = rep(1:2, each = 2), a = 1:4)
  df <- group_by(df, g)

  expect_identical(
    mutate(df, tibble(x = across2(where(is.numeric), where(is.numeric), sum)$a_a,
                      y = across2(where(is.numeric), where(is.numeric), max)$a_a)),
    mutate(df, x = sum(a) + sum(a), y = max(a))
  )
})

test_that("across2() rejects non vectors", {
  expect_error(
    data.frame(x = 1) %>% summarise(across2(everything(), everything(),  ~sym("foo")))
  )
  expect_error(
    data.frame(x = 1) %>% summarise(across2x(everything(), everything(),  ~sym("foo")))
  )
})

test_that("across2() uses tidy recycling rules", {
  expect_equal(
    data.frame(x = 1, y = 2) %>% summarise(across2(everything(), everything(), ~rep(42, .x))),
    data.frame(x_x = rep(42, 2), y_y = rep(42, 2))
  )

  expect_error(
    data.frame(x = 2, y = 3) %>% summarise(across2(everything(), everything(), ~rep(42, .x)))
  )
})

test_that("across2(<empty set>) returns a data frame with 1 row (#5204)", {
  df <- tibble(x = 1:42)
  expect_equal(
    mutate(df, across2(c(), c(), as.factor)),
    df
  )
  expect_equal(
    mutate(df, y = across2(c(), c(), as.factor))$y,
    tibble::new_tibble(list(), nrow = 42)
  )
  mutate(df, {
    res <- across2(c(), c(), as.factor)
    expect_equal(nrow(res), 1L)
    res
  })
})

# not supported yet
# test_that("across2(.names=) can use local variables in addition to {col} and {fn}", {
#   res <- local({
#     prefix <- "MEAN"
#     data.frame(x = 42) %>%
#       summarise(across2(everything(), mean, .names = "{prefix}_{.col}"))
#   })
#   expect_identical(res, data.frame(MEAN_x = 42))
# })

## resume here #####

# test_that("across2() uses environment from the current quosure (#5460)", {
#   # If the data frame `y` is selected, causes a subscript conversion
#   # error since it is fractional
#   df <- data.frame(x = 1, y = 2.4)
#   y <- "x"
#   expect_equal(df %>% summarise(across2(all_of(y), mean)), data.frame(x = 1))
#   expect_equal(df %>% mutate(across2(all_of(y), mean)), df)
#   expect_equal(df %>% filter(across2(all_of(y), ~ .x < 2)), df)
#
#   # Recursive case fails because the `y` column has precedence (#5498)
#   expect_error(df %>% summarise(summarise(across2(), across2(all_of(y), mean))))
#
#   # Inherited case
#   out <- df %>% summarise(local(across2(all_of(y), mean)))
#   expect_equal(out, data.frame(x = 1))
# })
#
# test_that("across2() sees columns in the recursive case (#5498)", {
#   df <- tibble(
#     vars = list("foo"),
#     data = list(data.frame(foo = 1, bar = 2))
#   )
#
#   out <- df %>% mutate(data = purrr::map2(data, vars, ~ {
#     .x %>% mutate(across2(all_of(.y), ~ NA))
#   }))
#   exp <- tibble(
#     vars = list("foo"),
#     data = list(data.frame(foo = NA, bar = 2))
#   )
#   expect_identical(out, exp)
#
#   out <- df %>% mutate(data = purrr::map2(data, vars, ~ {
#     local({
#       .y <- "bar"
#       .x %>% mutate(across2(all_of(.y), ~ NA))
#     })
#   }))
#   exp <- tibble(
#     vars = list("foo"),
#     data = list(data.frame(foo = 1, bar = NA))
#   )
#   expect_identical(out, exp)
# })
#
