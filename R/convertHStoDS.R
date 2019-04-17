#' Convert HS to DS
#'
#' This function converts HS to DS. Because the model used to convert values is
#' derived from \code{stats::loess model} model, it cannot make predictions
#' outside the range of the values used to construct the model. Our model was
#' tailored to the values frequently used by studies on individuality: \cr -
#' number of individuals should be between 5 and 40 individuals \cr - number of
#' observations per individual should be between 5 - 20 \cr - HS value should be
#' between 0 - 12.9 \cr\cr Consider increasing your sampling if number of
#' individuals and number of observations are lower than 5. If number of
#' individuals and observations exceeds the function limits, it might be
#' possible to use the largest possible values allowed by the model to get the
#' estimate - biases get smaller with large sampling, so, if your sampling
#' exceeds function limits a little bit, the estimate should be still quite
#' precise.
#'
#' @param nindivs Number of individuals. Must be within 5-40 individuals.
#' @param nobs Number of observations per individual. Must be within 5-20
#'   observations.
#' @param HS HS value to be converted into DS. Must be within 0-12.9 bits.
#' @param se should standard errors be computed (takes more time)?
#' @return If \code{se = FALSE}, Numeric value. DS for a specified number of
#'   individuals and number of observations per individual. \cr If \code{se = TRUE},
#'   a list containing components fit, se, residual.scale, df. See
#'   \code{predict.loess} for more details.
#' @examples
#' convertHStoDS(nindivs=10, nobs=10, HS=5)
#'
#' @family metric conversion
#' @export

convertHStoDS <- function(nindivs, nobs, HS, se=FALSE) {
  if( any(nindivs < 5 | nindivs > 40) ) stop('nindivs outside of range; nindivs must be within 5 and 40')
  if( any(nobs < 5 | nobs > 20) ) stop('nobs outside of range; nobs must be within 5 and 20')
  if( any(HS < 0 | HS > 12.9) ) stop('HS outside of range; HS must be within 0 and 12.9')

  datazpred <- data.frame(w=nindivs, x=nobs, z=HS)
  DSest <- stats::predict(HStoDSloess, datazpred, se=se)
  return(DSest)
}
