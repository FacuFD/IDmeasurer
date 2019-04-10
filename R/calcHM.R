#' Calculate information capacity (HM)
#'
#' This function calculates information capacity.
#'
#' @param df A dataframe with the first column indicating individual identity.
#' @return Numeric value. Individual identity information capacity HM (in bits) in dataset.
#' @examples
#' calcHM(ANmodulation)
#'
#' @family individual identity metrics
#' @seealso \code{\link{calcPIC}}, \code{\link{calcHS}}
#' @export

calcHM <- function(df){
  df[,1] <- as.factor(df[,1])
  Y <- df[,-1]
  MeanVec <- calcMeanVec(Y)
  distT <- calcDistT(df)
  distW <- calcDistW(df)

  g <- length(levels(df[,1]))
  npergroup <- length(df[,1]) / g
  FM <- ((npergroup-1) / (g-1)) * ((distT - g*(distW)) / distW)
  HM <- log2(sqrt(((FM+npergroup-1) / npergroup)))

  return(HM)
}








