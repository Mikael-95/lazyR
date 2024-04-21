#' HPDI
#'
#' @param samples The sampled values
#' @param prob Desired probability
#'
#' @return
#' @export

HPDI <- function( samples , prob=0.89 ) {
  # require(coda)
  coerce.list <- c( "numeric" , "matrix" , "data.frame" , "integer" , "array" )
  if ( inherits(samples, coerce.list) ) {
    # single chain for single variable
    samples <- coda::as.mcmc( samples )
  }
  x <- sapply( prob , function(p) coda::HPDinterval( samples , prob=p ) )
  # now order inside-out in pairs
  n <- length(prob)
  result <- rep(0,n*2)
  for ( i in 1:n ) {
    low_idx <- n+1-i
    up_idx <- n+i
    # lower
    result[low_idx] <- x[1,i]
    # upper
    result[up_idx] <- x[2,i]
    # add names
    names(result)[low_idx] <- concat("|",prob[i])
    names(result)[up_idx] <- concat(prob[i],"|")
  }
  return(result)
}

#' @notes
#' This function is from Richard McElreath's Rethinking package found in full here: https://github.com/rmcelreath/rethinking/
