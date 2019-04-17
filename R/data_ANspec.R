#' Little owl, \emph{Athene noctua} - spectrum properties
#'
#'\itemize{
#'  \item \strong{Species:} Little owl, \emph{Athene noctua}
#'  \item \strong{Number of individuals:} 33
#'  \item \strong{Number of calls per individual:} 10
#'  \item \strong{Number of acoustic variables:} 7
#'  \item \strong{Individual identity:} HS=4.68
#'  \item \strong{Reference:} Linhart, P., & Salek,
#' M. (2017). The assessment of biases in the acoustic discrimination of
#' individuals. PLOS ONE, 12(5), e0177206. doi:10.1371/journal.pone.0177206
#'}
#' Calls of little owls were collected in Czech Republic and Hungary.
#' Territorial calls of each male were recorded for three minutes after a short
#' playback provocation (1 min) inside their territories from up to 50 m
#' distance from the individuals. The recordings were made during comparable,
#' favourable meteorological conditions (without strong wind or precipitation),
#' from sunset until midnight between March and April of 2013 and 2014. This period
#' covered the mating season. The period and the time of the day for recording
#' were selected with regard to the peak in vocal activity of little owls both
#' within a day and within a season. The original dataset included 54 males with
#' more than 20 calls each (20-41 calls per individual, mean, SD = 26.9, 6.0)
#' with good recording quality.The number of individuals and calls per
#' individual was reduced to match parameters of the other datasets.\cr\cr
#' Variables were selected to measure basic parameters of call spectrum
#' like the peak frequency, distribution of frequency amplitudes within
#' spectrum, and range of the frequencies (minimum and maximum). Additionally,
#' the duration of the call was measured. Variables were extracted in SASLab Pro
#' by Avisoft.
#'

#'
#' @format A data frame with 330 rows and 8 variables:
#'
#' \describe{
#'   \item{id}{factor, identity code of an individual emitting the call}
#'   \item{dur}{duration of the call, in seconds}
#'   \item{df}{frequency of maximum amplitude within the spectrum - peak frequency, in Hertz}
#'   \item{minf, maxf}{minimum and maximum fequency at -25dB relative to the call peak amplitude, in Hertz}
#'   \item{q25, q50, q75}{frequencies at the three quartiles of amplitude
#'   distribution; frequencies below which lie 25, 50 and 75 percent of the energy of
#'   the call, respectively, in Hertz}
#' }
#' @source \href{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0177206#sec025}{Linhart, P., & Salek, M. (2017). The assessment of biases in the acoustic discrimination of
#' individuals. PLOS ONE, 12(5), e0177206. doi:10.1371/journal.pone.0177206}
"ANspec"
