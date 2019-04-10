#' Calculate F-values for individual identity traits
#'
#' This function calculates ANOVA F-values (type I sum of squares) for all
#' identity traits in dataset along with its significance. Each trait is used as
#' dependent and identity code as independent variable.
#'
#' @param df A dataframe with the first column indicating individual identity.
#' @return Data frame with 11 rows and 3 columns (trait, f-value, and p-value).
#' @examples
#' calcF(ANmodulation)
#'
#' @family individual identity metrics
#' @seealso \code{\link{calcPIC}}, \code{\link{calcHS}}
#' @export

calcF <- function(df) {
  df[,1] <- as.factor(df[,1])
  nvars <- length(names(df))-1
  f <- rep(NA, nvars)
  pvals <- rep(NA, nvars)
  for (i in 1:nvars){
    modelFormula <- paste(names(df)[i+1], '~', names(df)[1])
    f[i] <- summary(aov(as.formula(modelFormula), data=df))[[1]][["F value"]][[1]]
    pvals[i] <- summary(aov(as.formula(modelFormula), data=df))[[1]][["Pr(>F)"]][[1]]

  }
  traits <- names(df)[-1]
  f <- round(f, digits=2)
  pvals <- round(pvals, digits=3)
  result <- data.frame(traits, f, pvals)
  return(result)
}





