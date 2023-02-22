#' Remove Abnormal Values
#'
#' @param var A numeric, continuous variable given in df$var format.
#' @param min The lower threshold of expected values.
#' @param max The upper threshold of expected values.
#' @param data Source data frame
#'
#' @return A data frame.
#' @export


abnorm_rm <- function(var, data, min, max){
  ind <- which(var < min | var > max)
  n <- NROW(ind)

  low <- which(var < min)

  high <- which(var > max)

  cat("Removed", n, "rows", "\n")

  cat("Rows below", min, "=", NROW(low), "\n")

  cat("Rows above", max, "=", NROW(high), "\n")

  data <- data[-ind, ]
}
