# select_vars  ------------------------------------------------------------------

# select_vars examples of basic functionality from the example section

## examples
test_that("cut_names() simple exmamples", {

  cut_n1 <- cut_names("Width", .vars = names(iris))
  expect_equal(cut_n1, c("Sepal.", "Petal."))

  cut_n2 <- cut_names("Width", .vars = "Width.Petal.Width")
  expect_equal(cut_n2, c(".Petal."))

})

test_that("extract_names() simple exmamples", {

  extr_n1 <- extract_names("Length|Width", .vars = names(iris))
  expect_equal(extr_n1, c("Length", "Width"))

  extr_n2 <- extract_names("Width", .vars = "Width.Petal.Width")
  expect_equal(extr_n2, c("Width"))

})

test_that("cut_names() in over() (gets var names automatically)", {

  over_cut <- iris %>%
    dplyr::mutate(over(cut_names(".Width"),
                       ~ .("{.x}.Width") * .("{.x}.Length"),
                       .names = "Product_{x}"))

  expect_over_cut <- iris %>%
    dplyr::mutate(Product_Sepal = Sepal.Length * Sepal.Width,
                  Product_Petal = Petal.Length * Petal.Width)

  expect_equal(over_cut, expect_over_cut)

})

test_that("extract_names() in over() (gets var names automatically)", {

  over_extr <- iris %>%
    dplyr::mutate(over(extract_names("Length|Width"),
                       ~.("Petal.{.x}") * .("Sepal.{.x}"),
                       .names = "Product_{x}"))

  expect_over_extr <- iris %>%
    dplyr::mutate(Product_Length = Sepal.Length * Petal.Length,
                  Product_Width  = Sepal.Width * Petal.Width)

  expect_equal(over_extr, expect_over_extr)

})

test_that("extract_names() in over() with `.remove`", {

  over_extr <- csatraw %>%
    dplyr::transmute(over(extract_names("item\\d", "^item1"),
                          ~ .("{.x}a") * .("{.x}b"))
    )

  expect_named(over_extr, paste0("item", 2:6))

})

## more tests

test_that("cut_names() in over() with `.remove`", {

  over_cut <- csat %>%
    dplyr::transmute(over(cut_names("rating$", "^email"),
                          ~ paste0(.("{.x}rating"), .("{.x}contact")),
                          .names = "{x}new")
    )

  expect_named(over_cut, paste0(c("postal", "phone", "website", "shop"), "_new"))

})

