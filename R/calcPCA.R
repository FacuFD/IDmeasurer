#' Convert raw trait variables into principal components
#'
#' This function subjects the trait variables from the original dataset to the
#' Principal component analysis (PCA, stats:prcomp) and calculates principal
#' componenets scores for each sample. All variables are centered by subtracting
#' the variable mean from a particular value and scaled to the unit variance by
#' dividing the value by the standard deviation of a trait (stats::prcomp
#' parameters 'center = T', 'scale = T'). Some functions like, for example,
#' 'calcHS' require uncorrelated input variables to calculate individual
#' identity information properly.
#'
#' @param df A dataframe with the first column indicating individual identity.
#' @return df A data frame with the same attributes like the \code{df}, but the
#'   original individuality traits are replaced by principal components.
#' @examples
#' summary(ANmodulation)
#' temp <- calcPIC(ANmodulation)
#' summary(temp)
#'
#' @family data transformation
#' @seealso \code{\link{calcHS}}
#' @export


calcPCA <- function (df){
  id <- df[,1]
  pcscores <- stats::prcomp(df[,-1], center=T, scale=T, tol=0)$x
  temp <- data.frame(id,pcscores)
  return(temp)
}
