get_affix <- function(x, side = c("right", "left")) {

  x <- stringr::str_pad(x, max(nchar(x)), side = side, pad = " ")
  .variant <- purrr::transpose(strsplit(x, ""))
  .variant <- purrr::map_dbl(purrr::map(.variant, unique), length)
  .pre <- paste0(strsplit(x, "")[[1]][.variant == 1], collapse = "")
  .pre <- stringr::str_remove_all(.pre, "[:punct:]*$")
  .pre <- stringr::str_remove_all(.pre, "^[:punct:]*")
  .pre

}

# test_str <- c("Sepal.Length", "Sepal.Width")
# test_str <- c("Length.Sepal", "Width.Sepal")
#
# get_affix(test_str, "left")
