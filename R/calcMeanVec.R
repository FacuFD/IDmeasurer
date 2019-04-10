#' Calculate the centroid of the individual identity traits
#'
#' This function calculates centroid of the individual identity traits. Euclidean distances are used.
#'
#' @param df A dataframe with the individual identity traits without identity codes (NA will be produced for the column with identity code).
#' @return Numeric vector with the centroid values for each trait.
#' @examples
#' #incorrect use (with identity codes, NA will be produced):
#' calcMeanVec(ANmodulation)
#'
#' #correct use (with identity codes removed)
#' calcMeanvec(ANmodulation[-1])
#'
#' @family
#' @seealso

calcMeanVec <- function(df){
  nvars <- ncol(df)
  MeanVec <- rep(NA, nvars)

  for (i in 1:nvars) {
    MeanVec[i] <- mean(df[,i])
  }
  return(MeanVec)
}
