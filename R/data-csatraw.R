#' Customer Satisfaction Survey (raw data)
#'
#' This data is randomly generated. It resembles raw data from a customer
#' satisfaction survey using CSAT (Customer Satisfaction Score) for a
#' contract-based product. The first three variables are given, all other
#' variables come from a survey tool and are only named "item1" etc.
#' A recoded version of this data set can be found here <[`csat`][csat]>.
#'
#' @format A tibble with 150 rows and 15 variables:
#' \describe{
#' \item{cust_id}{Customer identification number}
#' \item{type}{Type of customer: "new", "existing" or "reactive"}
#' \item{product}{The type of product: "basic", "advanced" or "premium"}
#' \item{item1}{The overall Customer Satisfaction Score\cr\cr Scale: Ranging from 1 =
#'  "Very unsatisfied" to 5 = "Very satisfied"}
#' \item{item2a, item3a, item4a, item5a, item6a}{When did the customer have last
#'  contact via postal mail (item2a), phone (item3a), email (item4a), website
#'  (item5a), a retail shop (item6a) ?\cr\cr Scale: 0 = "no contact", 1 = "more
#'  than 3 years ago", 2 = "within 1 to 3 years", 3 = "within the last year"}
#' \item{item2b, item3b, item4b, item5b, item6b}{If customer had contact
#'  via postal mail (item2b), phone (item3b), email (item4b),  website (item5b),
#'  a retail shop (item6b): How satisfied was he?\cr\cr
#'  Scale: Ranging from 1 = "Very unsatisfied", to 5 = "Very satisfied"}
#' }
#' @examples
#' csatraw
"csatraw"
