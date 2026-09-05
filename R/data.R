#' Mandible lengths of male and female golden jackals
#'
#' Mandible lengths (in mm) for male and female golden jackals (*Canis aureus*)
#' from a collection of specimens in the British Museum of Natural History,
#' London, UK.
#'
#' @format A data frame with 20 observations on 2 variables:
#' \describe{
#'   \item{\code{Length}}{A numeric vector containing mandible lengths in mm.}
#'   \item{\code{Sex}}{A factor with levels \code{Male} and \code{Female}.}
#' }
#' @source The data were manually transcribed from Manly (2007).
#' @references
#' Higham, C.F.W., Kijngam, A., and Manly, B.F.J. (1980). An analysis of
#' prehistoric canid remains from Thailand. *Journal of Archaeological
#' Science*, **7**, 149--165.
#'
#' Manly, B.F.J. (2007). *Randomization, bootstrap and Monte Carlo methods in
#' biology*, third edition. Chapman & Hall/CRC, Boca Raton.
#' @examples
#' data(jackal)
#' str(jackal)
#'
#' ## boxplot of mandible length vs sex
#' plot(Length ~ Sex, data = jackal)
#' @keywords datasets
"jackal"
