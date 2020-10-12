## code to prepare `csatraw` dataset goes here

type_vc <- c(rep(c("new", "existing"), 2), "reactivate")
product_vc <- c("basic", "advanced", "premium")

set.seed(982342)

csatraw <- tibble::tibble(
  cust_id = stringr::str_pad(sample(150:99999, 150), 5, pad = "0"),
  type = sample(type_vc, 150, replace = TRUE),
  product = sample(product_vc, 150, replace = TRUE),
  item1  = round(runif(150, min = 1, max = 5), 0),
  item2a = sample(c(0,1,2,2,3,3), 150, replace = TRUE),
  item2b = ifelse(item2a == 0, NA, round(runif(150, min = 1, max = 5), 0)),
  item3a = sample(c(0,1,2,2,3,3), 150, replace = TRUE),
  item3b = ifelse(item3a  == 0, NA, round(runif(150, min = 1, max = 5), 0)),
  item4a = sample(c(0,1,2,2,3,3), 150, replace = TRUE),
  item4b = ifelse(item4a  == 0, NA, round(runif(150, min = 1, max = 5), 0)),
  item5a = sample(c(0,1,2,2,3,3), 150, replace = TRUE),
  item5b = ifelse(item5a  == 0, NA, round(runif(150, min = 1, max = 5), 0)),
  item6a = sample(c(0,1,2,2,3,3), 150, replace = TRUE),
  item6b = ifelse(item6a  == 0, NA, round(runif(150, min = 1, max = 5), 0))
)

usethis::use_data(csatraw, overwrite = TRUE)

csat <- csatraw

csat <- csat %>%
  rename(csat = item1) %>%
  rename_with(~ gsub("a", "_contact", .x),
              .cols = matches("\\da$")) %>%
  rename_with(~ gsub("b", "_rating", .x),
              .cols = matches("\\db$")) %>%
  rename_with(~ gsub("item2", "postal", .x),
              .cols = starts_with("item2")) %>%
  rename_with(~ gsub("item3", "phone", .x),
              .cols = starts_with("item3")) %>%
  rename_with(~ gsub("item4", "email", .x),
              .cols = starts_with("item4")) %>%
  rename_with(~ gsub("item5", "website", .x),
              .cols = starts_with("item5")) %>%
  rename_with(~ gsub("item6", "shop", .x),
              .cols = starts_with("item6")) %>%
  mutate(type = factor(type, levels = c("new", "existing", "reactivate")),
         product = factor(product, levels = c("basic", "advanced", "premium")),
         across(matches("csat") | ends_with("_rating"),
                ~ recode(.x,
                         `1` = "Very unsatisfied",
                         `2` = "Unsatisfied",
                         `3` = "Neutral",
                         `4` = "Satisfied",
                         `5` = "Very Satisfied")),
         across(ends_with("_contact"),
                ~ recode(.x,
                         `0` = "no contact",
                         `1` = "more than 3 years ago",
                         `2` = "within 1 to 3 years",
                         `3` = "within last year")))

usethis::use_data(csat, overwrite = TRUE)

