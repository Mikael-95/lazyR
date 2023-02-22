#' T-test Effect Size
#'
#' @param test A t-test saved as an object.
#'
#' @return Effect size (r) for the given test.
#' @export
t_effect_size <- function(test){
  t  <- test$statistic[[1]]
  df <- test$parameter[[1]]
  r  <- sqrt(t ^ 2/(t ^ 2 + df))
  cat("r =", r, "\n")
  cat("df =", df, "\n")
}
