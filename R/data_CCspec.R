#' Corncrake, \emph{Crex crex} - spectrum properties
#'
#'\itemize{
#'  \item \strong{Species:} Corncrake, \emph{Crex crex}
#'  \item \strong{Number of individuals:} 33
#'  \item \strong{Number of calls per individual:} 10
#'  \item \strong{Number of acoustic variables:} 7
#'  \item \strong{Individual identity:} HS=5.68
#'  \item \strong{Reference:} Budka, M., & Osiejuk, T. S. (2013). Formant
#'  Frequencies are Acoustic Cues to Caller Discrimination and are a Weak
#'  Indicator of the Body Size of Corncrake Males. Ethology, 119, 960-969.
#'  doi:10.1111/eth.12141
#'}
#' Corncrake calls were recorded at three sites in Poland and one in the Czech
#' Republic Recordings were made during the corncrake breeding season, from 8 to
#' 30 July, in 2011 and in 2012. Males were recorded when calling spontaneously,
#' in favourable conditions, at night (from 22.00 to 03.30, local time) from a
#' distance of ca. 5-10 m. The original dataset comprised 104 males with 10
#' calls measured from each male.\cr\cr
#' Seven variables were selected to measure duration of the first syllable of
#' the call and its basic spectral parameters of each first syllable of the call
#' like the peak frequency, distribution of frequency amplitudes within
#' spectrum, and range of the frequencies (minimum and maximum). Additionally,
#' the duration of the call was measured. Variables were extracted in SASLab Pro
#' by Avisoft.
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
#'
#' @source \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/eth.12141}{Budka, M., & Osiejuk, T. S. (2013). Formant Frequencies are Acoustic Cues to Caller Discrimination and are a Weak Indicator of the Body Size of Corncrake Males. Ethology, 119, 960-969. doi:10.1111/eth.12141}
"CCspec"
