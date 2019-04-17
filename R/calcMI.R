#' Calculate Mutual information (MI)
#'
#' This function calculates Mutual information of actual and predicted
#' individual identities in the dataset. It uses Linear discrimination analysis
#' (MASS::lda) to predict individual identity. Settings for LDA are identical to
#' those used in \code{calcDS} function, i.e., LDA uses leave-one-out
#' cross-validation and priors are equal for each individual in dataset. \cr\cr
#' \strong{Reference:} Mathevon, N., Koralek, A., Weldele, M., Glickman, S. E.,
#' & Theunissen, F. E. (2010). What the hyena’s laugh tells: Sex, age, dominance
#' and individual signature in the giggling call of Crocuta crocuta. BMC
#' Ecology, 10, 9-Article No.: 9. doi:10.1186/1472-6785-10-9 \cr\cr
#'
#' @param df A data frame with the first column indicating individual identity.
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
