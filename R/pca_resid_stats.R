#' Examine PCA Residuals
#'
#' @param data A correlation matrix.
#' @param loadings PCA loadings in PCA$loadings format.
#'
#' @return Overview of residual statistics.
#' @export
pca_resid_stats <- function(data, loadings){
  matrix        <- psych::factor.residuals(data, loadings)
  resids        <- as.matrix(matrix[upper.tri(matrix)])
  large.resid   <- abs(resids)>0.05
  numberLR      <- sum(large.resid)
  propLR        <- numberLR/nrow(resids)
  rmsr          <- sqrt(mean(resids^2))

  cat("Root means squared residual = ", rmsr, "\n")

  cat("Number of absolute residuals > 0.05 = ", numberLR, "\n")

  cat("Proportion of absolute residuals > 0.05 = ", propLR, "\n")

  hist(resids)
}
