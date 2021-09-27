# adapted from https://stackoverflow.com/a/60447909/9349302
is.date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}
