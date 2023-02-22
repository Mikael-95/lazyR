#' Remove Outliers Based on Z-Scores
#'
#' @param var - A continuous variable.
#' @param idvar - A participant ID variable.
#' @param condvar - A grouping variable indicating condition.
#' @param sd - Number of SDs outside of which outliers are defined.
#' @param data - A data frame containing the data.
#'
#' @return A new data frame trimmed of outliers.
#' @export

z_outlier <- function(var, idvar, condvar, sd, data){
  z     <- stats::ave(var, idvar, condvar, FUN = "scale")

  ind   <- which(z > sd | z < (-sd))

  n     <- NROW(ind)

  low   <- which(z < (-sd))

  high  <- which(z > sd)

  cat("Removed",n ,"rows", "\n")

  cat("Rows with Z above", sd,"=", NROW(high), "\n")

  cat("Rows with Z below -", sd, "=", NROW(low), "\n")

  data <- data[-ind, ]
}
