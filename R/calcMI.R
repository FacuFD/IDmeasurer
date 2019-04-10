#' Calculate Mutual information (MI)
#'
#' This function calculates Mutual information of actual and predicted
#' individual identities in the dataset. It uses Linear discrimination analysis
#' (MASS::lda) to predict individual identity. Settings for LDA are identical to
#' those used in \code{\link{calcDS}} function, i.e., LDA uses leave-one-out
#' cross-validation and priors are equal for each individual in dataset.
#'
#' @param df A dataframe with the first column indicating individual identity.
#' @return Numeric value of mutual information (in bits).
#' @examples
#' calcMI(ANmodulation)
#'
#' @family individual identity metrics
#' @seealso \code{\link{calcPIC}}, \code{\link{calcHS}}
#' @export

calcMI <- function(df){
  df[,1] <- as.factor(df[,1])
  Y <- df[-1]
  prior <- rep(1/nlevels(df[,1]), nlevels(df[,1]))
  lda.sim <- MASS::lda(Y, grouping=df[,1], CV=T, prior=prior)
  MI <- infotheo::mutinformation(df[,1], lda.sim$class) #the value in nats by default
  MI <- infotheo::natstobits(MI) #conversion to bits
  return(MI)
}
