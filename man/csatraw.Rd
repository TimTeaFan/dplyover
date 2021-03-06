% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-csatraw.R
\docType{data}
\name{csatraw}
\alias{csatraw}
\title{Customer Satisfaction Survey (raw data)}
\format{
A tibble with 150 rows and 15 variables:
\describe{
\item{cust_id}{Customer identification number}
\item{type}{Type of customer: "new", "existing" or "reactive"}
\item{product}{The type of product: "basic", "advanced" or "premium"}
\item{item1}{The overall Customer Satisfaction Score\cr\cr Scale: Ranging from 1 =
"Very unsatisfied" to 5 = "Very satisfied"}
\item{item1_open}{Follow-up question why the respondent gave this specific
Customer Satisfaction rating. The open-ended answers have been coded into six
categories: 11 = "great product", 12 = "good service", 13 = "friendly staff",
21 = "too expensive", 22 = "unfriendly", 23 = "no response" (multiple answers
possible).}
\item{item2a, item3a, item4a, item5a, item6a}{When did the customer have last
contact via postal mail (item2a), phone (item3a), email (item4a), website
(item5a), a retail shop (item6a) ?\cr\cr Scale: 0 = "no contact", 1 = "more
than 3 years ago", 2 = "within 1 to 3 years", 3 = "within the last year"}
\item{item2b, item3b, item4b, item5b, item6b}{If customer had contact
via postal mail (item2b), phone (item3b), email (item4b),  website (item5b),
a retail shop (item6b): How satisfied was he?\cr\cr
Scale: Ranging from 1 = "Very unsatisfied", to 5 = "Very satisfied"}
}
}
\usage{
csatraw
}
\description{
This data is randomly generated. It resembles raw data from a customer
satisfaction survey using CSAT (Customer Satisfaction Score) for a
contract-based product. The first three variables are given, all other
variables come from a survey tool and are only named "item1" etc.
A recoded version of this data set can be found here <\code{\link{csat}}>.
}
\examples{
csatraw
}
\keyword{datasets}
