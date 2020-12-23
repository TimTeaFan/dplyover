# over ------------------------------------------------------------------
library(dplyr)

# over examples of basic functionality from the example section
test_that("over() works on numeric vectors", {

  df0 <- tibble(x = 1:25)

  df_over <- df0 %>%
    mutate(over(c(1:3),
                ~ lag(x, .x)))

  df_expect <- df0 %>%
    mutate(`1` = lag(x, 1),
           `2` = lag(x, 2),
           `3` = lag(x, 3))

  expect_equal(df_over, df_expect)

})

test_that("over() works on character vectors", {

  df_over <- iris %>%
    mutate(over(unique(Species),
                ~ if_else(Species == .x, 1, 0)),
                .keep = "none")

  df_expect <- iris %>%
    mutate(setosa = if_else(Species == "setosa", 1, 0),
           versicolor = if_else(Species == "versicolor", 1, 0),
           virginica = if_else(Species == "virginica", 1, 0),
           .keep = "none")

  expect_equal(df_over, df_expect)

})

test_that("over() can control names", {

  df_over <- iris %>%
    mutate(over(seq(4, 7, by = 1),
                ~ if_else(Sepal.Length < .x, 1, 0),
               .names = "Sepal.Length_{x}"),
               .keep = "none")

  df_expect <- iris %>%
    mutate(over(seq(4, 7, by = 1),
                ~ if_else(Sepal.Length < .x, 1, 0),
                .names = "Sepal.Length_{x}"),
           .keep = "none")

  expect_equal(df_over, df_expect)

})

test_that("over() works with dates & can transform names ", {


  dat_tbl <- tibble::tibble(start = seq.Date(as.Date("2020-01-01"),
                                     as.Date("2020-01-15"),
                                     by = "days"),
                            end = start + 10)

  df_over <- dat_tbl %>%
    mutate(over(seq(as.Date("2020-01-01"),
                    as.Date("2020-01-21"),
                    by = "weeks"),
                ~ .x >= start & .x <= end,
                .names = "day_{x}",
                .names_fn = ~ gsub("-", "", .x)))

  df_expect <- dat_tbl %>%
    mutate(day_20200101 = "2020-01-01" >= start & "2020-01-01" <= end,
           day_20200108 = "2020-01-08" >= start & "2020-01-08" <= end,
           day_20200115 = "2020-01-15" >= start & "2020-01-15" <= end)

  expect_equal(df_over, df_expect)

})


test_that("over() works with summarise", {

df_over <- csatraw %>%
  group_by(type) %>%
  summarise(over(c(1:5),
                 ~ mean(item1 == .x)))

df_expect <- csatraw %>%
  group_by(type) %>%
  summarise(`1` = mean(item1 == 1),
            `2` = mean(item1 == 2),
            `3` = mean(item1 == 3),
            `4` = mean(item1 == 4),
            `5` = mean(item1 == 5))

expect_equal(df_over, df_expect)

})

test_that("over() works with named lists", {

  df_over <- csatraw %>%
    group_by(type) %>%
    summarise(over(list(bot2 = c(1:2),
                        mid  = 3,
                        top2 = c(4:5)),
                   ~ mean(item1 %in% .x)))

  df_expect <- csatraw %>%
    group_by(type) %>%
    summarise(`bot2` = mean(item1 %in% 1:2),
              `mid`  = mean(item1 %in% 3),
              `top2` = mean(item1 %in% 4:5))

  expect_equal(df_over, df_expect)

})

test_that("over() works with a data.frame", {

  recode_df <- data.frame(old  = c(1, 2, 3, 4, 5),
                          top1 = c(0, 0, 0, 0, 1),
                          top2 = c(0, 0, 0, 1, 1),
                          bot1 = c(1, 0, 0, 0, 0),
                          bot2 = c(1, 1, 0, 0, 0))


  df_over <- csatraw %>%
    transmute(over(recode_df[,-1],
                   ~ .x[match(item1, recode_df[, 1])],
                   .names = "item1_{x}"))

  df_expect <- csatraw %>%
    transmute(
      item1_top1 = recode(item1, `1` = 0, `2` = 0, `3` = 0, `4` = 0, `5` = 1),
      item1_top2 = recode(item1, `1` = 0, `2` = 0, `3` = 0, `4` = 1, `5` = 1),
      item1_bot1 = recode(item1, `1` = 1, `2` = 0, `3` = 0, `4` = 0, `5` = 0),
      item1_bot2 = recode(item1, `1` = 1, `2` = 1, `3` = 0, `4` = 0, `5` = 0)
      )

  expect_equal(df_over, df_expect)

})

test_that("over() works with list-columns", {

  df_over <- csat %>%
    transmute(over(unique(unlist(csat_open)),
                ~ as.integer(grepl(.x, csat_open)),
                .names = "rsp_{x}",
                .names_fn = ~ gsub("\\s", "_", .x)))


  df_expect <- csat %>%
    select(cust_id, csat_open) %>%
    tidyr::unnest(csat_open) %>%
    mutate(val = 1L) %>%
    tidyr::pivot_wider(id_cols = cust_id,
                       names_from = csat_open,
                       values_from = val,
                       values_fill = 0L) %>%
    select(-cust_id) %>%
    rename_with(~ paste0("rsp_", gsub("\\s", "_", .x)))

  expect_equal(df_over, df_expect)

})


test_that("over() works string evaluation", {

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

  df_over2 <- iris %>%
    mutate(over(c("Sepal", "Petal"),
                ~ eval(sym(paste0(.x, ".Width"))) +
                  eval(sym(paste0(.x, ".Length")))
    ))

  expect_equal(df_over2, df_expect)

})

test_that("over() works with anonymous functions", {

  df_over <-  iris %>%
    summarise(over(c("Sepal", "Petal"),
                   function(x) mean(.("{x}.Width"))
                   ))

  df_expect <- iris %>%
    summarise(
      Sepal = mean(Sepal.Width),
      Petal = mean(Petal.Width)
    )

  expect_equal(df_over, df_expect)

})

test_that("over() works named lists", {

  df_over <-  iris %>%
    mutate(over(c("Sepal", "Petal"),
                list(product = ~ .("{.x}.Width") * .("{.x}.Length"),
                     sum = ~ .("{.x}.Width") + .("{.x}.Length"))),
           .keep = "none")

  df_expect <- iris %>%
    mutate(
      Sepal_product = Sepal.Width * Sepal.Length,
      Sepal_sum = Sepal.Width + Sepal.Length,
      Petal_product = Petal.Width * Petal.Length,
      Petal_sum = Petal.Width + Petal.Length,
      .keep = "none"
    )

  expect_equal(df_over, df_expect)

})

test_that("over() works named lists", {

  df_over <-  iris %>%
    mutate(over(c("Sepal", "Petal"),
                list(product = ~ .("{.x}.Width") * .("{.x}.Length"),
                     sum = ~ .("{.x}.Width") + .("{.x}.Length"))),
           .keep = "none")

  df_expect <- iris %>%
    mutate(
      Sepal_product = Sepal.Width * Sepal.Length,
      Sepal_sum = Sepal.Width + Sepal.Length,
      Petal_product = Petal.Width * Petal.Length,
      Petal_sum = Petal.Width + Petal.Length,
      .keep = "none"
    )

  expect_equal(df_over, df_expect)

})

test_that("over() can control names", {

  df_over <-  iris %>%
    mutate(over(c("Sepal", "Petal"),
                list(product = ~ .("{.x}.Width") * .("{.x}.Length"),
                     sum = ~ .("{.x}.Width") + .("{.x}.Length")),
                .names = "{fn}_{x}"),
           .keep = "none")

  df_expect <- iris %>%
    mutate(
      product_Sepal = Sepal.Width * Sepal.Length,
      sum_Sepal = Sepal.Width + Sepal.Length,
      product_Petal = Petal.Width * Petal.Length,
      sum_Petal = Petal.Width + Petal.Length,
      .keep = "none"
    )

  expect_equal(df_over, df_expect)

})

# tests adopted from across
test_that("over() works on one column data.frame", {

  df0 <- data.frame(x = 1)

  df_over <- df0 %>%
    mutate(over(1, ~ x * .x))

  df_exepect <- df0 %>%
    mutate(`1` = x * 1)

  expect_equal(df_over, df_exepect)

})

test_that("over() does not select grouping variables", {

  df0 <- data.frame(g = 1, x = 1)

  df_over <- df0 %>%
    group_by(g) %>%
    summarise(x = over(1, ~ x * .x)) %>%
    pull()

  expect_equal(df_over, tibble(`1` = 1))

})

test_that("over() correctly names output columns", {
  gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

  expect_named(
    mutate(gf, over(1, ~ x * .x)),
    c("x", "y", "z", "s", "1")
  )
  expect_named(
    mutate(gf, over(1, ~ x * .x, .names = "id_{x}")),
    c("x", "y", "z", "s", "id_1")
  )
  expect_named(
    summarise(gf, over(1, ~ mean(x + .x), .names = "mean_{x}")),
    c("x", "mean_1")
  )
  expect_named(
    summarise(gf, over(1, list(mean = mean, sum = sum))),
    c("x", "1_mean", "1_sum")
  )
  expect_named(
    summarise(gf, over(1, list(mean = mean, sum))),
    c("x", "1_mean", "1_2")
  )
  expect_named(
    summarise(gf, over(1, list(mean, sum = sum))),
    c("x", "1_1", "1_sum")
  )
  expect_named(
    summarise(gf, over(1, list(mean, sum))),
    c("x", "1_1", "1_2")
  )
  expect_named(
    summarise(gf, over(1, list(mean = mean, sum = sum), .names = "{fn}_{x}")),
    c("x", "mean_1", "sum_1")
  )
  # further added over()'s x_val, x_idx, x_nm
  expect_named(
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = "{fn}_{x_val}")),
    c("x", "mean_5", "sum_5",  "mean_6", "sum_6", "mean_7", "sum_7")
  )
  expect_warning(
    summarise(gf, over(list(a = 5:6, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = "{fn}_{x_val}"))
  )
  expect_warning(
    summarise(gf, over(data.frame(a = 5:6, b = 6:7, c = 7:8),
                       list(mean = mean, sum = sum),
                       .names = "{fn}_{x_val}"))
  )
  expect_named(
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = "{fn}_{x_nm}")),
    c("x", "mean_a", "sum_a",  "mean_b", "sum_b", "mean_c", "sum_c")
  )
  expect_warning(
    summarise(gf, over(list(5, 6, 7),
                       list(mean = mean, sum = sum),
                       .names = "{fn}_{x_nm}"))
  )
  expect_named(
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = "{fn}_{x_idx}")),
    c("x", "mean_1", "sum_1",  "mean_2", "sum_2", "mean_3", "sum_3")
  )
  expect_error(
    summarise(gf, over(list(a = 5, b = 5, c = 7),
                       list(mean = mean, sum = sum),
                       .names = "{fn}_{x_val}"))
  )
  expect_error(
    summarise(gf, over(list(a = 5, a = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = "{fn}_{x_nm}"))
  )
  expect_error(
    summarise(gf, over(list(a = 5, a = 6, c = 7),
                       list(mean = mean, sum = sum)))
  )
  # further added external vector
  col_nm_vec <- c("one", "two", "three", "four", "five", "six")
  expect_named(
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = col_nm_vec)),
    c("x", "one", "two", "three", "four", "five", "six")
  )
  # test that external vector throws error when too short
  col_nm_vec2 <- c("one", "two", "three")
  expect_error(
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = col_nm_vec2))
  )
  # test that external vector throws error when too long
  col_nm_vec3 <- c("one", "two", "three", "four", "five", "six", "seven")
  expect_error(
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = col_nm_vec3))
  )
  # test that external vectors throws error when it contains non-unique names
  col_nm_vec4 <- rep(col_nm_vec2 <- c("one", "two", "three"), 2)
  expect_error(
    summarise(gf, over(list(a = 5, b = 6, c = 7),
                       list(mean = mean, sum = sum),
                       .names = col_nm_vec4))
  )

})

test_that("over() result locations are aligned with .fn list names", {

  df <- tibble(x = 1:2)

  expect <- tibble(`1_cls` = "integer", `1_type` = TRUE,
                   `2_cls` = "integer", `2_type` = TRUE)

  x <- summarise(df, over(1:2, list(cls = ~ class(x + .x),
                                    type = ~ is.numeric(x + .x))))
  expect_identical(x, expect)
})

test_that("over() passes ... to functions", {

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


# issues not adapted in over code yet

# test_that("over(.names=) can use local variables in addition to {col} and {fn}", {
#   res <- local({
#     prefix <- "MEAN"
#     data.frame(x = 42) %>%
#       summarise(over(0, ~ mean(x + .x), .names = "{prefix}_{x}"))
#   })
#   expect_identical(res, data.frame(MEAN_x = 42))
# })
#
# test_that("over() uses environment from the current quosure (#5460)", {
#   # If the data frame `y` is selected, causes a subscript conversion
#   # error since it is fractional
#   df <- data.frame(x = 1, y = 2.4)
#   y <- "x"
#   expect_equal(df %>% summarise(over(.env$y, mean, .names = "x_idx")), data.frame(x = 1))
#   expect_equal(df %>% mutate(over(all_of(y), mean)), df)
#   expect_equal(df %>% filter(over(all_of(y), ~ .x < 2)), df)
#
#   # Recursive case fails because the `y` column has precedence (#5498)
#   expect_error(df %>% summarise(summarise(across(), across(all_of(y), mean))))
#
#   # Inherited case
#   out <- df %>% summarise(local(across(all_of(y), mean)))
#   expect_equal(out, data.frame(x = 1))
# })

# test_that("across() sees columns in the recursive case (#5498)", {
#   df <- tibble(
#     vars = list("foo"),
#     data = list(data.frame(foo = 1, bar = 2))
#   )
#
#   out <- df %>% mutate(data = purrr::map2(data, vars, ~ {
#     .x %>% mutate(across(all_of(.y), ~ NA))
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
#       .x %>% mutate(across(all_of(.y), ~ NA))
#     })
#   }))
#   exp <- tibble(
#     vars = list("foo"),
#     data = list(data.frame(foo = 1, bar = NA))
#   )
#   expect_identical(out, exp)
# })


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
