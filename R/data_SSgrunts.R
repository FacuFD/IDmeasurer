#' Domestic pig, \emph{Sus scrofa domestica} - piglet grunts
#'
#'\itemize{
#'  \item \strong{Species:} Domestic pig, \emph{Sus scrofa domestica}
#'  \item \strong{Number of individuals:} 33
#'  \item \strong{Number of calls per individual:} 10
#'  \item \strong{Number of acoustic variables:} 10
#'  \item \strong{Individual identity:} HS=2.57
#'  \item \strong{Reference:} Syrova, M., Policht, R., Linhart, P., & Spinka, M.
#'  (2017). Ontogeny of individual and litter identity signaling in grunts of
#'  piglets. The Journal of the Acoustical Society of America, 142(5),
#'  3116-3121. doi:10.1121/1.5010330
#'}
#' Piglet grunts were recorded in 2014 and 2015 at the research farm of
#' Institute of Animal Science in Prague. To elicit the calls piglets were
#' separated from the litter and sow and were recorded in social isolation in
#' age of 25-30 days after birth from the distance of 1m. Piglets were recorded
#' opportunistically during the day. The original dataset comprised 97 piglets
#' coming from 13 different litters and 10 calls per individual piglet.\cr\cr
#' Variables were selected to be the most informative regarding the individual
#' identity by the reference study. Variables were extracted using the LMA 2008
#' software for analysis of animal sounds by Kurt Hammerschmidt.
#'
#' @format A data frame with 330 rows and 8 variables:
#'
#' \describe{
#'   \item{id}{factor, identity code of an individual emitting the call}
#'   \item{fp1amean}{mean amplitude of the 1st frequency peak, relative amplitude}
#'   \item{q1mean}{mean frequency of the 1st DFreqA; equivalent to q25 in ANspec, CCspec, and LAhighweewoo, in Hertz}
#'   \item{csmean}{mean correlation coefficient of successive time segments}
#'   \item{tonalF0}{estimation of segments with detectable fundamental frequency, in percents}
#'   \item{q3mean}{mean frequency of the 3nd DFreqA; equivalent to q75 in ANspec, CCspec, and LAhighweewoo, in Hertz}
#'   \item{noise}{percentage of noise time segments, in percents}
#'   \item{ranmean}{mean frequency range, in Hertz}
#'   \item{q2mean}{mean frequency of the 2nd DFreqA; equivalent to q50 in ANspec, CCspec, and LAhighweewoo, in Hertz}
#'   \item{df2pr}{percentage of time segments where 2nd DomFreqB detected, in percents}
#'   \item{q2min}{min frequency of the 2nd DFreqA, in Hertz}
#' }
#' @source \href{https://asa.scitation.org/doi/10.1121/1.5010330}{Syrova, M., Policht, R., Linhart, P., & Spinka, M. (2017). Ontogeny of individual and litter identity signaling in grunts of piglets. The Journal of the Acoustical Society of America, 142(5),  3116-3121. doi:10.1121/1.5010330}
"SSgrunts"
