#' Calculate Beecher's information statistic (HS, variant = HSvarcomp)
#'
#' This function calculates Beecher's information statistic (HS) for all
#' variables within the dataset. This function is calculated using equations
#' from Beecher, M. D. (1989). Signaling Systems for Individual Recognition - an
#' Information-Theory Approach. Animal Behaviour, 38, 248-261.
#' doi:10.1016/S0003-3472(89)80087-9. \cr\cr 'calcHS' (equivalent to 'calcHSnpergroup') is the
#' correct variant of the function calculating Beechers information statistic. The other
#' variants use total sample size (calcHSntot) or number of individuals in dataset (calcHSngroups) instead of
#' number of samples per individual to calculate HS. 'calcHSvarcomp' calculates
#' HS from variance components of mixed models. HS values calculated by
#' 'calcHSvarcomp' were found to be twice as large compared to HS calculated by standard
#' approach. \cr\cr Please note, 'sumHS = TRUE' should be used in datasets where
#' individuality traits are uncorrelated. If traits are correlated, Principal
#' component analysis (PCA) should be applied and HS should be calculated on
#' uncorrelated principal componenets instead of original trait variables.
#'
#' @param df A dataframe with the first column indicating individual identity.
#' @param sumHS Logical. 'sumHS = TRUE' (default) will sum partial HS values of
#'   each trait variable; 'sumHS = FALSE' provides partial HS values separately for each
#'   individuality trait in a dataset.
#' @return For 'sumHS = TRUE': Numeric vector of two elements indicating indicating: 1) HS summed
#'   over variables that significantly differ between individuals (in one-way
#'   Anova with individual as independent and a specific signal trait as
#'   dependent variable; or 2) HS summed over all variables in dataset.
#'
#'   For 'sumHS = FALSE': Data frame with thre columns and number of rows equal to
#'   number of variables in dataset. First column includes names of traits
#'   considered for individuality. Second column includes significance test for
#'   each trait (from one-way ANOVA with individual identity as independent
#'   factor and trait as dependent variable). Third column includes values of HS
#'   for each variable trait.
#'
#' @examples
#' calcHSvarcomp(ANmodulation)
#' temp <- calcPCA(ANmodulation)
#' calcHSvarcomp(temp)
#'
#' @family individual identity metrics
#' @seealso \code{\link{calcPIC}}, \code{\link{calcHS}}
#' @export

calcHSvarcomp <- function (df, sumHS=T) {

    nvars <- ncol(df)
    vars <- names(df)
    HS <- rep(NA, nvars)

    randomVar <- rep(NA,nvars)
    residVar <- rep(NA,nvars)
    totalVar <- rep(NA,nvars)

    for (k in 2:nvars) {
      modelFormula <- paste0(vars[k], '~1+(1|', vars[1], ')')
      lmer.m1 <- lme4::lmer(stats::as.formula(modelFormula), data=df)
      randomVar[k] <- (as.data.frame(lme4::VarCorr(lmer.m1))[1,4])
      residVar[k] <- as.data.frame(lme4::VarCorr(lmer.m1))[2,4]
      totalVar[k] <- randomVar[k]+residVar[k]
      HS[k] <- log2((totalVar[k] / residVar[k]))
    }

    HS <- round(HS, 2)
    result <- data.frame(vars,HS)

    if (sumHS==T) {
      result <- c(sum(result$HS,na.rm=T))
      names(result) <- c('HS for all vars')
      return (result)
    } else return (result)
}
