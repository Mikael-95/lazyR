#' Lazy Posterior Probability Proportions
#'
#' @param var The variable or parameter to be quantified
#' @param data The dataframe containing the variable or parameter. This will most commonly be a dataframe of posterior samples.
#'
#' @return Probability rations above and below zero for the desired parameter.
#' @export
lazy_post_ratio <- function(var, data) {
  cat::cat("Parameter:", base::deparse(base::substitute(var)), "\n")
  cat::cat("Proportion above zero:", base::round(base::sum(var > 0) / base::NROW(data), digits = 2), "\n")
  cat::cat("Proportion below zero:", base::round(base::sum(var < 0) / base::NROW(data), digits = 2), "\n")
}
