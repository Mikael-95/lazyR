#' Lazy Contrasts
#'
#' @param var The variable to apply contrasts to
#' @param contrasts The contrasts to be applied, e.g.,  cbind(c(1, -1)) for simple sum orthogonal contrasts for a factor of two levels.
#'
#' @return contrasts with descriptive outputs.
#' @export

lazy_contrasts <- function(var, contrasts){

var <- base::factor(var)
base::levels(var)
stats::contrasts(var) <- contrasts
base::attr(var, "contrasts")
}
