#' Customer Satisfaction Survey (recoded data)
#'
#' This data is randomly generated. It resembles data from a customer
#' satisfaction survey using CSAT (Customer Satisfaction Score) for a
#' contract-based product. The data has been recoded. The raw version of this data
#' set can be found here <[`csatraw`][csatraw]>.
#'
#' @format A tibble with 150 rows and 15 variables:
#' \describe{
#' \item{cust_id}{Customer identification number}
#' \item{type}{Type of customer: "new", "existing" or "reactive"}
#' \item{product}{The type of product: "basic", "advanced" or "premium"}
#' \item{csat}{The overall Customer Satisfaction Score}
#' \item{csat_open}{Follow-up question why the respondent gave this specific
#' Customer Satisfaction rating. The open-ended answers have been coded into six
#' categories (multiple answers possible).}
#' \item{postal_contact, phone_contact, email_contact, website_contact,
#'  shop_contact}{When did the customer have last contact via given channel?}
#' \item{postal_rating, phone_rating, email_rating, website_rating,
#'  shop_rating}{If customer had contact over the given channel:
#'  How satisfied was he?}
#' }
#' @examples
#' csat
"csat"
