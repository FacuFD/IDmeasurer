#' Generate dataset with a single individual identity trait
#'
#' This functions generates a dataset with desired parameters (number of
#' individuals and number of observations per individual, mean of the parameter,
#' individuality).

#' @param nindivs Indicates how many individuals should be in dataset
#' @param nobs Indicates how many observations per individual should be in dataset
#' @param betweenM Indicates the mean value of the trait.
#' @param individuality Indicates the ratio of between to within individual
#'   variation.
#' @return Data frame with two columns. Identity codes are in the first column
#'   and the trait values are in the second column.
#' @examples
#' GenerateUnivariate(nindivs=10, nobs=10, betweenM=1000, individuality=2)
#'
#' @family Operations with datasets
#' @export

GenerateUnivariate <- function(nindivs,nobs,betweenM, individuality) {

  id <- c()
  paramvec <- c()
  means <- stats::rnorm(nindivs, betweenM, 1)
  indcodes <- seq(from=1, to=nindivs, by=1)
  indsds <- means # just to build same matrix as for means
  indsds <- rep(stats::sd(means)/individuality, nindivs) # calculates sd of means for each variable and sets the individual sd to be individuality times the sd between means


  for (i in 1:nindivs) {
    paramvec <- c(paramvec, stats::rnorm(nobs, means[i], indsds[i]))
    id <- c(id, rep(indcodes[i], nobs))
  }

  temp <- data.frame(id, paramvec) # pair the matrix with individual codes
  temp$id <- as.factor(temp$id) # individual is factor
  return(temp)

}


