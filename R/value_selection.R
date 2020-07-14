get_values <- function(.var, .dat = NULL) {

  if (is.null(.dat) && sys.call(sys.nframe() - 1)[[1]] == "over_setup") {
    .dat <- across()
  }

  var <- rlang::as_string(rlang::ensym(.var))

  unique(as.character(.dat[[var]]))

}

chr_sq <- function(from, to, by) {
  as.character(seq.int(from, to, by = by))
}

num <- function(x) {
  as.numeric(x)
}

