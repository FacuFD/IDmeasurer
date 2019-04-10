#' Calculate total distance in given dataset
#'
#' This function calculates centroid of all samples in a given dataset and sums
#' distances between the centroid and each sample. Euclidean distances are used.
#'
#' @param df A dataframe with the first column indicating individual identity.
#' @return Numeric. Total distance.
#' @examples
#' calcDistT(ANmodulation)
#'
#' @family
#' @seealso

calcDistT <- function(df){
  Y <- df[-1]
  MeanVec <- calcMeanVec(Y)
  MeanVecDist <- rep(NA, nrow(Y))
  for (i in 1:nrow(Y)){
    MeanVecDist[i] <- sum((MeanVec-Y[i,])^2) * (1/length(MeanVec))
  }
  TotalDist <- sum(MeanVecDist)
  return(TotalDist)
}
