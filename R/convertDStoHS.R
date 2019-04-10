#' Convert DS to HS
#'
#' This function converts DS to HS. Because the model used to convert values is
#' derived from loess model, it cannot make predictions outside the range of the
#' values used to construct the loess model. The model was tailored to the
#' sampling frequently used by studies on individuality: \cr - number of
#' individuals should be between 5 and 40 individuals \cr - number of
#' observations per individual should be between 5 - 20 \cr - DS value should be
#' between 0 - 1 \cr\cr Consider increasing your sampling if number of
#' individuals and/or number of observations are lower than 5. If number of
#' individuals and observations exceeds the function limits, it might be
#' acceptable to use the largest possible values allowed by the model to get the
#' estimate - biases get smaller with larger sampling, so, if your sampling
#' exceeds function limits a little bit, the estimate should be still quite
#' precise.
#'
#' @param nindivs Number of individuals. Must be within 5-40 individuals.
#' @param nobs Number of observations per individual. Must be within 5-20 observations.
#' @param DS DS value to be converted into HS. Must be within 0-1.
#' @param se should standard errors be computed (takes more time)?
#' @return If \code{se = FALSE}, Numeric value. DS for a specified number of
#'   individuals and number of observations per individual. \cr If \code{se = TRUE},
#'   a list containing components fit, se, residual.scale, df. See
#'   \code{predict.loess} for more details.
#' @examples
#' convertDStoHS(nindivs=10, nobs=10, DS=0.7)
#'
#' @family metric conversion
#' @seealso \code{\link{calcPIC}}, \code{\link{calcHS}}
#' @export

convertDStoHS <- function(nindivs, nobs, DS, se=FALSE) {
  if( any(nindivs < 5 | nindivs > 40) ) stop('nindivs outside of range; nindivs must be within 5 and 40')
  if( any(nobs < 5 | nobs > 20) ) stop('nobs outside of range; nobs must be within 5 and 20')
  if( any(DS < 0 | DS > 1) ) stop('DS outside of range; DS must be within 0 and 1')

  datazpred <- data.frame(w=nindivs, x=nobs, y=DS)
  HSest <- predict(DStoHSloess, datazpred, se=se)
  return(HSest)
}
