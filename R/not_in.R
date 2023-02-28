#' Not In
#'
#' @param x What to search for
#' @param y What do search in
#'
#' @return a logical vector of whether values of x are not found in y
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(1, 2, 3, 4, 6)
#'
#' x %!in% y
#'
`%!in%` <- function(x, y) {
  !(x %in% y)
  }
