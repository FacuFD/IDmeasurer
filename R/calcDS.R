#' Calculates discrimination score (DS)
#'
#' This function will take the specified data frame and will perform linear
#' discrimination analysis with leave-one-out crossvalidation and equal priors
#' for each individual (e.g., all priors will be set to 1/10 in case that the
#' dataset contains 10 individuals). Variables are not modified in any way;
#' scaling, centering, transformation of variables, or principal component
#' analysis, etc., if required, need to be done on dataset before calling this
#' function. \cr\cr \strong{Reference:} e.g. Hafner, G. W., Hamilton, C. L.,
#' Steiner, W. W., Thompson, T. J., & Winn, H. E. (1979). Signature information
#' in the song of the humpback whale. Journal of the Acoustical Society of
#' America, 66, 1-6. doi:10.1121/1.383072 \cr\cr
#'
#' @param df A data frame with the first column noting individual identity of the sample.
#' @return Proportion of samples correctly classified by LDA in \code{df}.
#' @examples
#' calcDS(ANmodulation)
#'
#' @family individual identity metrics
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
