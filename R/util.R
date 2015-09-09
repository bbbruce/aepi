#' Internal function to cross multiply a 2 x 2 table
#' @param x table to cross multiply
crossmul <- function(x) {
  (x[1, 1] * x[2, 2]) / (x[1, 2] * x[2, 1])
}

#' Internal function that creates a strata name from row of expand.grid
strata.name <- function(x) {
  x <- sapply(x, as.character)
  paste(names(x), x, sep = " = ", collapse = ", ")
}

#' Internal function to extract a stratum from a table based on a row of expand.grid
strata.extract <- function(x, tab) {
  x <- sapply(x, as.character)
  l <- list(tab, 1:2, 1:2)
  l <- c(l, x)
  do.call(`[`, l)
}
