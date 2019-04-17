#' Calculates potential of identity coding (PIC, variant=PICbetweenmeans)
#'
#' This function calculates Potential of individual coding for all variables
#' within the dataset. \cr\cr \strong{Reference:} Robisson, P. (1992). Roles of
#' pitch and duration in the discrimination of the mate's call in the King
#' penguin Aptenodytes patagonicus. Bioacoustics, 4, 25-36. \cr\cr
#'
#' @param df A data frame with the first column indicating individual identity.
#' @return Numeric vector with PIC values for each variable in \code{df}.
#' @examples
#' calcPICbetweenmeans(ANmodulation)
#'
#' @family individual identity metrics
#' @export

calcPICbetweenmeans <- function(df) {
  df[,1] <- as.factor(df[,1])
  nvars <- length(names(df))-1
  PIC <- rep(NA, nvars)

  for (i in 1:nvars){
    grouping <- df[,1]
    x <- df[,i+1]
    idmean <- tapply(x, grouping, mean)
    idsd <- tapply(x, grouping, stats::sd)
    idn <- tapply(x, grouping, length)
    idwcv <- idsd / idmean
    #idwcv2 <- 100 * (1 + 1/(4*idn)) * (idsd / idmean)  # correction for low sample sizes
    meanWCV <- mean(idwcv, na.rm=T)
    #meanWCV2 <- mean(idwcv2, na.rm=T)   # correction for low sample size

    BCV <- stats::sd(idmean,na.rm=T) / mean(idmean, na.rm=T) ### Coefficient of variation between individuals calculated from individual means

    PIC[i] <- BCV / meanWCV
    #PIC[i] <- (100 * BCV * (1 + 1/(4*length(idmean)))) / meanWCV2  # correction for low sample size
  }

  names(PIC) <- names(df[-1])
  return(PIC)
}
