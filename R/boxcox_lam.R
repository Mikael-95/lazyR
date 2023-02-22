#' Box-Cox Lambda Overview
#'
#' @param var A continuous variable.
#' @param formula a regression formula, raw text or stored as an object, using lm().
#' @param data Original dataframe.
#'
#' @return A plot and table of lambda values.
#' @export

boxcox_lam <- function(var, formula, data){
  graphics::par(mfrow = c(1,2), mar = c(4,4,2,0.5), font.axis = 1, cex.axis = 1)
  bRT <- MASS::boxcox(formula, data = data)
  lambda <- bRT$x;
  lik = bRT$y;
  bc = cbind(lambda, lik)
  output <- bc[order(-lik),]
  plot(stats::density(var))
  print(utils::head(output))
}
