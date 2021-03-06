#' Little owl, \emph{Athene noctua} - frequency modulation
#'
#'\itemize{
#'  \item \strong{Species:} Little owl, \emph{Athene noctua}
#'  \item \strong{Number of individuals:} 33
#'  \item \strong{Number of calls per individual:} 10
#'  \item \strong{Number of acoustic variables:} 11
#'  \item \strong{Individual identity:} HS=3.48
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
#' Eleven variables were measured to describe the modulation of the fundamental
#' frequency within the call (shape of the call on spectrogram) and the duration
#' of the call. Fundamental frequency was measured at ten measuring points
#' spread equidistantly throughout the call duration. Variables were extracted
#' in SASLab Pro by Avisoft.\cr
#'

#'
#' @format A data frame with 330 rows and 12 variables:
#'
#' \describe{
#'   \item{id}{factor, identity code of an individual emitting the call}
#'   \item{f1, f3, f5, f7, f9, f11, f13, f15, f17, f19}{fundamental frequency at the specific measuring point, in Hertz}
#'   \item{dur}{numeric, duration of the call, in seconds}
#' }
#' @source \href{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0177206#sec025}{Linhart, P., & Salek, M. (2017). The assessment of biases in the acoustic discrimination of
#' individuals. PLOS ONE, 12(5), e0177206. doi:10.1371/journal.pone.0177206}
"ANmodulation"
