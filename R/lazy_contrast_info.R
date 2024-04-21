#' Lazy Contrast Info
#'
#' @param var The variable to apply contrasts to
#' @param contrasts The contrasts to be applied, e.g.,  cbind(c(1, -1)) for simple sum orthogonal contrasts for a factor of two levels.
#'
#' @return contrasts with descriptive outputs.
#' @export

lazy_contrast_info <- function(var){

base::levels(var)
base::attr(var, "contrasts")
}
library()
