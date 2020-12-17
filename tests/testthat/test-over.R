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
    unnest(csat_open) %>%
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

# resume from here
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

# Resume here

test_that("across() result locations are aligned with column names (#4967)", {
  df <- tibble(x = 1:2, y = c("a", "b"))
  expect <- tibble(x_cls = "integer", x_type = TRUE, y_cls = "character", y_type = FALSE)

  x <- summarise(df, across(everything(), list(cls = class, type = is.numeric)))

  expect_identical(x, expect)
})

test_that("across() passes ... to functions", {
  df <- tibble(x = c(1, NA))
  expect_equal(
    summarise(df, across(everything(), mean, na.rm = TRUE)),
    tibble(x = 1)
  )
  expect_equal(
    summarise(df, across(everything(), list(mean = mean, median = median), na.rm = TRUE)),
    tibble(x_mean = 1, x_median = 1)
  )
})

test_that("across() passes unnamed arguments following .fns as ... (#4965)", {
  df <- tibble(x = 1)
  expect_equal(mutate(df, across(x, `+`, 1)), tibble(x = 2))
})

test_that("across() avoids simple argument name collisions with ... (#4965)", {
  df <- tibble(x = c(1, 2))
  expect_equal(summarize(df, across(x, tail, n = 1)), tibble(x = 2))
})

test_that("across() works sequentially (#4907)", {
  df <- tibble(a = 1)
  expect_equal(
    mutate(df, x = ncol(across(where(is.numeric))), y = ncol(across(where(is.numeric)))),
    tibble(a = 1, x = 1L, y = 2L)
  )
  expect_equal(
    mutate(df, a = "x", y = ncol(across(where(is.numeric)))),
    tibble(a = "x", y = 0L)
  )
  expect_equal(
    mutate(df, x = 1, y = ncol(across(where(is.numeric)))),
    tibble(a = 1, x = 1, y = 2L)
  )
})

test_that("across() retains original ordering", {
  df <- tibble(a = 1, b = 2)
  expect_named(mutate(df, a = 2, x = across())$x, c("a", "b"))
})

test_that("across() gives meaningful messages", {
  verify_output(test_path("test-across-errors.txt"), {
    tibble(x = 1) %>%
      summarise(res = across(where(is.numeric), 42))

    across()
    c_across()
  })
})

test_that("monitoring cache - across() can be used twice in the same expression", {
  df <- tibble(a = 1, b = 2)
  expect_equal(
    mutate(df, x = ncol(across(where(is.numeric))) + ncol(across(a))),
    tibble(a = 1, b = 2, x = 3)
  )
})

test_that("monitoring cache - across() can be used in separate expressions", {
  df <- tibble(a = 1, b = 2)
  expect_equal(
    mutate(df, x = ncol(across(where(is.numeric))), y = ncol(across(a))),
    tibble(a = 1, b = 2, x = 2, y = 1)
  )
})

test_that("monitoring cache - across() usage can depend on the group id", {
  df <- tibble(g = 1:2, a = 1:2, b = 3:4)
  df <- group_by(df, g)

  switcher <- function() {
    if_else(cur_group_id() == 1L, across(a)$a, across(b)$b)
  }

  expect <- df
  expect$x <- c(1L, 4L)

  expect_equal(
    mutate(df, x = switcher()),
    expect
  )
})

test_that("monitoring cache - across() internal cache key depends on all inputs", {
  df <- tibble(g = rep(1:2, each = 2), a = 1:4)
  df <- group_by(df, g)

  expect_identical(
    mutate(df, tibble(x = across(where(is.numeric), mean)$a, y = across(where(is.numeric), max)$a)),
    mutate(df, x = mean(a), y = max(a))
  )
})

test_that("across() rejects non vectors", {
  expect_error(
    data.frame(x = 1) %>% summarise(across(everything(), ~sym("foo")))
  )
})

test_that("across() uses tidy recycling rules", {
  expect_equal(
    data.frame(x = 1, y = 2) %>% summarise(across(everything(), ~rep(42, .))),
    data.frame(x = rep(42, 2), y = rep(42, 2))
  )

  expect_error(
    data.frame(x = 2, y = 3) %>% summarise(across(everything(), ~rep(42, .)))
  )
})

test_that("across(<empty set>) returns a data frame with 1 row (#5204)", {
  df <- tibble(x = 1:42)
  expect_equal(
    mutate(df, across(c(), as.factor)),
    df
  )
  expect_equal(
    mutate(df, y = across(c(), as.factor))$y,
    tibble::new_tibble(list(), nrow = 42)
  )
  mutate(df, {
    res <- across(c(), as.factor)
    expect_equal(nrow(res), 1L)
    res
  })
})


# expected errors



# other edge cases
