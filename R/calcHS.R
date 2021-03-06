#' Calculate Beecher's information statistic (HS, variant = HSnpergroup)
#'
#' This function calculates Beecher's information statistic (HS) for all
#' variables within the dataset. \cr\cr \strong{Reference:} Beecher, M. D.
#' (1989). Signaling Systems for Individual Recognition - an Information-Theory
#' Approach. Animal Behaviour, 38, 248-261. doi:10.1016/S0003-3472(89)80087-9.
#' \cr\cr \code{calcHS} (equivalent to \code{calcHSnpergroup}) is the correct
#' variant of the function calculating Beechers information statistic. The other
#' variants use total sample size (\code{calcHSntot}) or number of individuals
#' in dataset (\code{calcHSngroups}) instead of number of samples per individual
#' to calculate HS. \code{calcHSvarcomp} calculates HS from variance components
#' of mixed models. HS values calculated by \code{calcHSvarcomp} were found to
#' be twice as large compared to HS calculated by standard approach. \cr\cr
#' Please note, \code{sumHS = TRUE} should be used in datasets where
#' individuality traits are uncorrelated. If traits are correlated, Principal
#' component analysis (PCA) should be applied and HS should be calculated on
#' uncorrelated principal componenets instead of original trait variables.
#'
#' @param df A data frame with the first column indicating individual identity.
#' @param sumHS \code{sumHS = TRUE} (default) will sum partial HS values of each
#'   trait variable; \code{sumHS = FALSE} provides partial HS values separately
#'   for each individuality trait in a dataset.
#' @return For \code{sumHS = TRUE}: Numeric vector of two elements indicating
#'   indicating: 1) HS summed over variables that significantly differ between
#'   individuals (in one-way Anova with individual as independent and a specific
#'   signal trait as dependent variable; or 2) HS summed over all variables in
#'   dataset.
#'
#'   For \code{sumHS = FALSE}: Data frame with thre columns and number of rows
#'   equal to number of variables in dataset. First column includes names of
#'   traits considered for individuality. Second column includes significance
#'   test for each trait (from one-way ANOVA with individual identity as
#'   independent factor and trait as dependent variable). Third column includes
#'   values of HS for each variable trait.
#'
#' @examples
#' calcHS(ANmodulation)
#' temp <- calcPCA(ANmodulation)
#' calcHS(temp)
#'
#' @family individual identity metrics
#' @export

calcHS <- function (df, sumHS = TRUE){

  nvars <- ncol(df)
  vars <- names(df)
  n <- nrow(df)
  indivs <- levels(as.factor(df[,1]))
  nindiv <- length(indivs)
  npergroup <- nrow(df) / nindiv
  fvalues <- rep(NA, nvars)
  Pr <- rep(NA, nvars)
  HS <- rep(NA, nvars)

  for (k in 2:nvars) {
    modelFormula <- paste(vars[k], '~', vars[1])
    fvalues [k] <- summary(stats::aov(stats::as.formula(modelFormula), data=df))[[1]][["F value"]][[1]]
    Pr[k] <- summary(stats::aov(stats::as.formula(modelFormula), data=df))[[1]][["Pr(>F)"]][[1]]
    HS [k] <- log2(sqrt((fvalues[k]+(npergroup-1)) / npergroup)) #npergroup
  }
  Pr <- round(Pr, 3)
  HS <- round(HS, 2)
  result <- data.frame(vars,Pr,HS)
  result <- result[-1,]

  if (sumHS==T) {
    result <- c(sum(result$HS[result$Pr<0.05],na.rm=T), sum(result$HS,na.rm=T))
    names(result) <- c('HS for significant vars', 'HS for all vars')
    return (result)
  } else return (result)
}
