#' Yellow-breasted boubou, \emph{Laniarius atroflavus} - spectrum properties
#'
#'\itemize{
#'  \item \strong{Species:} Yellow-breasted boubou, \emph{Laniarius atroflavus}
#'  \item \strong{Number of individuals:} 33
#'  \item \strong{Number of calls per individual:} 10
#'  \item \strong{Number of acoustic variables:} 6
#'  \item \strong{Individual identity:} HS=3.83
#'  \item \strong{Reference:} Osiejuk, unpublished data
#'}
#' Male Yellow-breasted boubous were recorded in Bamenda region in Cameroon.
#' Birds were recorded between 06.00 to 10.00 in the morning in 2016, typically,
#' from the distance of 10 - 20 meters. The calls were recorded after short
#' provocation with playback. Repertoire of males at the field site included
#' three distinct call types and only the most common call typed labeled as
#' “High wee woo” was used for this study. The original dataset comprised 33
#' individuals and 10 calls per individual. \cr\cr
#' Variables were selected to measure basic spectral parameters of each “high
#' weewoo” call like the peak frequency, distribution of frequency amplitudes
#' within spectrum, and range of the frequencies (minimum and maximum).
#' Additionally, the duration of the call was measured. Variables were extracted
#' in Raven Pro 1.5 by the Cornell Bioacoustic Research Program.
#'
#' @format A data frame with 330 rows and 7 variables:
#'
#' \describe{
#'   \item{id}{factor, identity code of an individual emitting the call}
#'   \item{dur}{duration of the call, in seconds}
#'   \item{df}{frequency of maximum amplitude within the spectrum - peak frequency, in Hertz}
#'   \item{minf, maxf}{minimum and maximum fequency at -25dB relative to the call peak amplitude, in Hertz}
#'   \item{q25, q75}{frequencies at the two quartiles of amplitude
#'   distribution; frequencies below which lie 25 and 75 percent of the energy of
#'   the call, respectively, in Hertz}
#' }
#' @source Osiejuk, unpublished data
"LAhighweewoo"
