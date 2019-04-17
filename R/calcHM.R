#' Calculate information capacity (HM)
#'
#' This function calculates information capacity of a signal. \cr\cr
#' \strong{Reference:} Searby, A., & Jouventin, P. (2004). How to measure
#' information carried by a modulated vocal signature? Journal of the Acoustical
#' Society of America, 116, 3192-3198. doi:10.1121/1.1775271 \cr\cr
#'
#' @param df A data frame with the first column indicating individual identity.
#' @return Numeric value. Individual identity information capacity HM (in bits) in dataset.
#' @examples
#' calcHM(ANmodulation)
#'
#' @family individual identity metrics
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








