#' Calculates discrimination score (DS)
#'
#' This function will take the specified dataframe and will perform linear
#' discrimination analysis with leave-one-out crossvalidation and equal priors
#' for each individual (e.g., all priors will be set to 1/10 in case that
#' the dataset contains 10 individuals). Variables are not modified in any way;
#' scaling, centering, transformation of variables, or principal component
#' analysis, etc., if required, need to be done on dataset before calling this
#' function.
#'
#' @param df A dataframe with the first column noting individual identity of the sample.
#' @return Proportion of samples correctly classified by LDA in \code{df}.
#' @examples
#' calcDS(ANmodulation)
#'
#' @family individual identity metrics
#' @seealso \code{\link{calcPIC}}, \code{\link{calcHS}}
#' @export


calcDS <- function (df){
  df[,1] <- as.factor(df[,1])
  Y <- df[-1]
  prior <- rep(1/nlevels(df[,1]), nlevels(df[,1]))
  lda.sim <- MASS::lda(Y, grouping=df[,1], CV=T, prior=prior)
  ct <- table(df[,1], lda.sim$class)
  result <- sum(diag(prop.table(ct)))
  return(result)
}
