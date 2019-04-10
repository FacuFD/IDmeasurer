#' Calculate average within individual distance
#'
#' This function calculates centroid for each individual and sums distances of
#' samples from centroid for that particular individual. When the within
#' individual sum of distances is known for each individual, it calculates their
#' mean. Euclidean distances are used.
#'
#' @param df A dataframe with the first column indicating individual identity.
#' @return Numeric. Average within individual distance in dataset.
#' @examples
#' calcDistW(ANmodulation)
#'
#' @family
#' @seealso

calcDistW <- function (df) {
  indivs <- levels(df[,1])
  nindivs <- length(indivs)
  DistW <- rep(NA, nindivs)
  for (i in 1:nindivs){
    df.id <- df[df[,1]==indivs[i],]
    DistW[i] <- calcDistT(df.id)
  }
  return(mean(DistW))
}
